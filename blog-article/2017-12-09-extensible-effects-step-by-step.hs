#!/usr/bin/env stack
{- stack repl
   --resolver lts-14.0
   --package extensible-0.6.1
   --package containers
   --package mtl
   --package monad-logger
-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

import Data.Extensible
import Data.Extensible.Effect.Default

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LoggingT, MonadLogger, monadLoggerLog, runStdoutLoggingT)
import Control.Monad.Reader (ask, local)
import Control.Monad.State (MonadState, get, put)
import Control.Monad.Writer (tell)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

type Name = String

data Exp
  = Lit Integer
  | Var Name
  | Plus Exp Exp
  | Abs Name Exp
  | App Exp Exp
  deriving Show

data Value
  = IntVal Integer
  | FunVal Env Name Exp
  deriving Show

type Env = Map Name Value

eval0 :: Env -> Exp -> Value
eval0 env (Lit i)      = IntVal i
eval0 env (Var n)      = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) = IntVal (i1 + i2)
  where
    IntVal i1 = eval0 env e1
    IntVal i2 = eval0 env e2
eval0 env (Abs n e)   = FunVal env n e
eval0 env (App e1 e2) =
  case val1 of
    FunVal env' n body -> eval0 (Map.insert n val2 env') body
  where
    val1 = eval0 env e1
    val2 = eval0 env e2

---

type Eval1 a = Eff '[] a

runEval1 :: Eval1 a -> a
runEval1 = leaveEff

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) =
  case Map.lookup n env of
    Nothing  -> fail ("undefined variable: " `mappend` n)
    Just val -> return val
eval1 env (Plus e1 e2) = do
  e1' <- eval1 env e1
  e2' <- eval1 env e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _                      -> fail "type error in addition"
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do
  val1 <- eval1 env e1
  val2 <- eval1 env e2
  case val1 of
    FunVal env' n body -> eval1 (Map.insert n val2 env') body
    _                  -> fail "type error in application"

---

type Eval2 a = Eff '[ EitherDef String ] a

runEval2 :: Eval2 a -> Either String a
runEval2 = leaveEff . runEitherDef

eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) =
  case Map.lookup n env of
    Nothing  -> throwError ("unbound variable: " `mappend` n)
    Just val -> return val
eval2 env (Plus e1 e2) = do
  e1' <- eval2 env e1
  e2' <- eval2 env e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _                      -> throwError "type error in addition"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2) = do
  val1 <- eval2 env e1
  val2 <- eval2 env e2
  case val1 of
      FunVal env' n body -> eval2 (Map.insert n val2 env') body
      _                  -> throwError "type error in application"

---

type Eval3 a = Eff '[ ReaderDef Env, EitherDef String ] a

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env = leaveEff . runEitherDef . flip runReaderDef env

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do
  env <- ask
  case Map.lookup n env of
    Nothing  -> throwError ("unbound variable: " `mappend` n)
    Just val -> return val
eval3 (Plus e1 e2) = do
  e1' <- eval3 e1
  e2' <- eval3 e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _                      -> throwError "type error in addition"
eval3 (Abs n e) = do
  env <- ask
  return $ FunVal env n e
eval3 (App e1 e2) = do
  val1 <- eval3 e1
  val2 <- eval3 e2
  case val1 of
    FunVal env' n body -> local (const (Map.insert n val2 env')) (eval3 body)
    _                  -> throwError "type error in application"

---

type Eval4 a = Eff '[ ReaderDef Env, EitherDef String, StateDef Integer ] a

runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env st = leaveEff . flip runStateDef st . runEitherDef . flip runReaderDef env

tick :: (Num s, MonadState s m) => m ()
tick = do
  st <- get
  put (st + 1)

eval4 :: Exp -> Eval4 Value
eval4 (Lit i) = do
  tick
  return $ IntVal i
eval4 (Var n) = do
  tick
  env <- ask
  case Map.lookup n env of
    Nothing  -> throwError ("unbound variable: " `mappend` n)
    Just val -> return val
eval4 (Plus e1 e2) = do
  tick
  e1' <- eval4 e1
  e2' <- eval4 e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _                      -> throwError "type error in addition"
eval4 (Abs n e) = do
  tick
  env <- ask
  return $ FunVal env n e
eval4 (App e1 e2) = do
  tick
  val1 <- eval4 e1
  val2 <- eval4 e2
  case val1 of
    FunVal env' n body -> local (const (Map.insert n val2 env')) (eval4 body)
    _                  -> throwError "type error in application"

---

type Eval5 a = Eff '[ ReaderDef Env, EitherDef String, WriterDef [String], StateDef Integer ] a

runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env st = leaveEff . flip runStateDef st . runWriterDef . runEitherDef . flip runReaderDef env

eval5 :: Exp -> Eval5 Value
eval5 (Lit i) = do
  tick
  return $ IntVal i
eval5 (Var n) = do
  tick
  tell [n]
  env <- ask
  case Map.lookup n env of
    Nothing  -> throwError ("unbound variable: " `mappend` n)
    Just val -> return val
eval5 (Plus e1 e2) = do
  tick
  e1' <- eval5 e1
  e2' <- eval5 e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _                      -> throwError "type error in addition"
eval5 (Abs n e) = do
  tick
  env <- ask
  return $ FunVal env n e
eval5 (App e1 e2) = do
  tick
  val1 <- eval5 e1
  val2 <- eval5 e2
  case val1 of
    FunVal env' n body -> local (const (Map.insert n val2 env')) (eval5 body)
    _                  -> throwError "type error in application"

---

type Eval6 a = Eff '[ ReaderDef Env, EitherDef String, WriterDef [String], StateDef Integer, "IO" >: IO ] a

runEval6 :: Env -> Integer -> Eval6 a -> IO ((Either String a, [String]), Integer)
runEval6 env st = retractEff . flip runStateDef st . runWriterDef . runEitherDef . flip runReaderDef env

eval6 :: Exp -> Eval6 Value
eval6 (Lit i) = do
  tick
  liftIO $ print i
  return $ IntVal i
eval6 (Var n) = do
  tick
  tell [n]
  env <- ask
  case Map.lookup n env of
    Nothing  -> throwError ("unbound variable: " `mappend` n)
    Just val -> return val
eval6 (Plus e1 e2) = do
  tick
  e1' <- eval6 e1
  e2' <- eval6 e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _                      -> throwError "type error in addition"
eval6 (Abs n e) = do
  tick
  env <- ask
  return $ FunVal env n e
eval6 (App e1 e2) = do
  tick
  val1 <- eval6 e1
  val2 <- eval6 e2
  case val1 of
    FunVal env' n body -> local (const (Map.insert n val2 env')) (eval6 body)
    _                  -> throwError "type error in application"

---

type Eval a = Eff '[ LoggerDef, "IO" >: IO ] a

runEval :: Eval a -> IO a
runEval = retractEff . runLoggerDef

type LoggerDef = "Logger" >: Logging
type Logging = LoggingT IO

runLoggerDef :: (MonadIO (Eff xs)) => Eff (LoggerDef ': xs) a -> Eff xs a
runLoggerDef = peelEff0 pure $ \m k -> k =<< liftIO (runStdoutLoggingT m)

instance (Lookup xs "Logger" Logging) => MonadLogger (Eff xs) where
  monadLoggerLog loc ls level = liftEff (Proxy @"Logger") . monadLoggerLog loc ls level

liftIO' :: IO a -> Eval a
liftIO' = liftEff (Proxy @"Logger") . liftIO

---

main :: IO ()
main = do
  let
    goodExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
    badExp1 = (Plus (Lit 1) (Abs "x" (Var "x")))
    badExp2 = (Var "x")

  print $ eval0 Map.empty goodExp

  print $ runEval1 $ eval1 Map.empty goodExp

  print $ runEval2 $ eval2 Map.empty goodExp
  print $ runEval2 $ eval2 Map.empty badExp1
  print $ runEval2 $ eval2 Map.empty badExp2

  print $ runEval3 Map.empty $ eval3 goodExp
  print $ runEval3 Map.empty $ eval3 badExp1
  print $ runEval3 Map.empty $ eval3 badExp2

  print $ runEval4 Map.empty 0 $ eval4 goodExp
  print $ runEval4 Map.empty 0 $ eval4 badExp1
  print $ runEval4 Map.empty 0 $ eval4 badExp2

  print $ runEval5 Map.empty 0 $ eval5 goodExp
  print $ runEval5 Map.empty 0 $ eval5 badExp1
  print $ runEval5 Map.empty 0 $ eval5 badExp2

  print =<< (runEval6 Map.empty 0 $ eval6 goodExp)
  print =<< (runEval6 Map.empty 0 $ eval6 badExp1)
  print =<< (runEval6 Map.empty 0 $ eval6 badExp2)
