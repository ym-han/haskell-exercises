module Lesson3 where


-- Exercise: write a re-orderd tuple functor such tt fmap (+1) (3, 4) returns (4,4) rather than (3,5)
data FlippedTup right left = FT left right deriving (Eq, Show)

instance Functor (FlippedTup l) where
    -- fmap :: (a -> b) -> FlippedTup l a -> FlippedTup l b 
    fmap g (FT left right) = FT (g left) right


