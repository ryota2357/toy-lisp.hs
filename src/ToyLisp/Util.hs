module ToyLisp.Util where

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

assertAlways :: Bool -> a -> a
assertAlways True  x = x
assertAlways False _ = error "Assertion failed"
