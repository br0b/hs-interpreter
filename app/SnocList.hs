module SnocList where

import Control.Applicative (Alternative)
import GHC.Base (empty, (<|>))

newtype SnocList a = SnocList {unSnocList :: [a]}

toList :: SnocList a -> [a]
toList = reverse . unSnocList

fromList :: [a] -> SnocList a
fromList = SnocList . reverse

snoc :: SnocList a -> a -> SnocList a
snoc (SnocList xs) x = SnocList (x : xs)

instance Eq a => Eq (SnocList a) where
  (==) :: SnocList a -> SnocList a -> Bool
  slx == sly = unSnocList slx == unSnocList sly

instance Show a => Show (SnocList a) where
  show :: SnocList a -> String
  show = show . unSnocList

instance Semigroup (SnocList a) where
  (<>) :: SnocList a -> SnocList a -> SnocList a
  slx <> sly = SnocList $ unSnocList sly <> unSnocList slx

instance Monoid (SnocList a) where
  mempty :: SnocList a
  mempty = SnocList []

instance Functor SnocList where
  fmap :: (a -> b) -> SnocList a -> SnocList b
  fmap f sl = SnocList $ f <$> unSnocList sl

instance Applicative SnocList where
  pure :: a -> SnocList a
  pure x = SnocList [x]

  (<*>) :: SnocList (a -> b) -> SnocList a -> SnocList b
  SnocList (f : fs) <*> SnocList (x : xs) =
    snoc (SnocList fs <*> SnocList xs) (f x)
  SnocList _ <*> _ = SnocList []

instance Alternative SnocList where
  empty :: SnocList a
  empty = SnocList []

  (<|>) :: SnocList a -> SnocList a -> SnocList a
  slx <|> sly = SnocList $ unSnocList sly ++ unSnocList slx
