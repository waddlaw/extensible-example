module Main (main) where

data Nat = Zero | Succ Nat

instance Eq Nat where
  (Succ n) == (Succ m) = n == m
  Zero == Zero = True
  _ == _ = False

instance Show Nat where
  show Zero = "Zero"
  show (Succ n) = "Succ " ++ show n

-- |
-- >>> add (Succ (Succ (Succ Zero))) (Succ (Succ Zero))
-- Succ (Succ (Succ (Succ (Succ Zero))))
add :: Nat -> Nat -> Nat
add Zero m = m
add n Zero = n
add (Succ n) m = add n (Succ m)

-- |
-- >>> sub (Succ (Succ (Succ Zero))) (Succ (Succ Zero))
-- Succ Zero
sub :: Nat -> Nat -> Maybe Nat
sub n Zero = Just n
sub Zero _ = Nothing
sub (Succ n) (Succ m) = sub n m

-- |
-- >>> mul (Succ (Succ (Succ Zero))) (Succ (Succ Zero))
-- Succ (Succ (Succ (Succ (Succ (Succ Zero)))))
mul :: Nat -> Nat -> Nat
mul n Zero = Zero
mul Zero m = Zero
mul n (Succ m) = add n (mul n m)

foldN :: a -> (a -> a) -> Nat -> a
foldN z s Zero = z
foldN z s (Succ n) = s (foldN z s n)

main :: IO ()
main = do
  let
    n = Succ (Succ (Succ Zero))
    m = Succ (Succ Zero)
  print $ add n m
  print $ sub n m
  print $ mul n m
  print $ n == m
  print $ n /= m