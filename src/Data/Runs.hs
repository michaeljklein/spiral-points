
module Data.Runs where

import Data.Semigroup
import Control.Monad
import qualified Data.List.NonEmpty as N

-- | Lists with runs of equal values replaced with
-- @(number of elements in the run, the repeated value)@
newtype Runs a = Runs { getRuns :: [(Int, a)] } deriving (Eq, Ord, Show)

instance Foldable Runs where
  foldMap _ (Runs []) = mempty
  foldMap f ~(Runs ((n, x):xs)) = stimes n (f x) <> foldMap f (Runs xs)

-- | Convert a list to `Runs`, one half of an isomorphism:
--
-- @
--  forall a. Eq a => Iso' [a] (Runs a)
-- @
--
toRuns :: Eq a => [a] -> Runs a
toRuns = Runs . fmap (liftM2 (,) N.length N.head) . N.group

