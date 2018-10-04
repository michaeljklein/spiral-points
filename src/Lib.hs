
-----------------------------------------------------------------------------
-- |
-- Module      :  Lib
-- Copyright   :  (C) 2018 Michael J. Klein
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  lambdamichael@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Want:
-- Efficient (I think we can get near constant-time) conversion between a point
-- @(Integer, Integer)@ and its "spiral number": `Natural`
----------------------------------------------------------------------------
module Lib where

import Numeric.Natural
import Data.Coerce
import GHC.Read
import qualified Data.List.NonEmpty as N
import Control.Monad (liftM2)
import Data.Foldable
import Data.Semigroup


-- | A point in @Z^2@
newtype Pt = Pt { getPr :: (Integer, Integer) } deriving (Eq, Ord)

instance Show Pt where
  showsPrec n ~(Pt pt) = showsPrec n pt
  show ~(Pt pt) = show pt
  showList = showList . (coerce `asTypeOf` fmap getPr)

instance Read Pt where
  readPrec = (coerce `asTypeOf` fmap Pt) readPrec
  readListPrec = (coerce `asTypeOf` fmap (fmap Pt)) readListPrec

-- | Elementwise addition, multiplication, etc.
--
-- `fromInteger` provided by `unSpiral`
instance Num Pt where
  ~(Pt (x, y)) + ~(Pt (z, w)) = Pt (x + z, y + w)
  ~(Pt (x, y)) * ~(Pt (z, w)) = Pt (x * z, y * w)
  ~(Pt (x, y)) - ~(Pt (z, w)) = Pt (x - z, y - w)
  abs ~(Pt (x, y)) = Pt (abs x, abs y)
  signum ~(Pt (x, y)) = Pt (signum x, signum y)
  fromInteger x | x < 0 = error $ unwords ["Pt: fromInteger: negative input:", show x]
                | otherwise = unSpiral $ fromInteger x

-- | `undefined`
--
-- Convert a `Natural` number to its point on the spiral
unSpiral :: Natural -> Pt
unSpiral = undefined

-- | `undefined`
--
-- Convert a point to its position on the spiral
toSpiral :: Pt -> Natural
toSpiral ~(Pt (x, y)) = undefined x y

-- | `undefined`
--
-- Get the next position on the spiral
stepSpiral :: Pt -> Pt
stepSpiral ~(Pt (x, y)) = undefined x y

-- | Spiral example
--
-- @
--  (20, -2,  2) (21, -1,  2) (22, 0,  2) (23, 1,  3) (24, 2,  2) (25, 3,  2)
--  (19, -2,  1) ( 6, -1,  1) ( 7, 0,  1) ( 8, 1,  1) ( 9, 2,  1) (26, 3,  1)
--  (18, -2,  0) ( 5, -1,  0) ( 0, 0,  0) ( 1, 1,  0) (10, 2,  0) (27, 3,  0)
--  (17, -2, -1) ( 4, -1, -1) ( 3, 0, -1) ( 2, 1, -1) (11, 2, -1) (28, 3, -1)
--  (16, -2, -2) (15, -1, -2) (14, 0, -2) (13, 1, -2) (12, 2, -2) (29, 3, -2)
--  (35, -2, -3) (34, -1, -3) (33, 0, -3) (32, 1, -3) (31, 2, -3) (30, 3, -3)
-- @
--
spiralExample :: ()
spiralExample = ()

-- | Unwrapped spiral example:
--
-- @
--  (20, -2,  2)
--  (19, -2,  1)
--  (18, -2,  0)
--  (17, -2, -1)
--  (16, -2, -2)
--  (21, -1,  2)
--  ( 6, -1,  1)
--  ( 5, -1,  0)
--  ( 4, -1, -1)
--  (15, -1, -2)
--  (22, 0,  2)
--  ( 7, 0,  1)
--  ( 0, 0,  0)
--  ( 3, 0, -1)
--  (14, 0, -2)
--  (23, 1,  3)
--  ( 8, 1,  1)
--  ( 1, 1,  0)
--  ( 2, 1, -1)
--  (13, 1, -2)
--  (24, 2,  2)
--  ( 9, 2,  1)
--  (10, 2,  0)
--  (11, 2, -1)
--  (12, 2, -2)
--  (25, 3,  2)
--  (26, 3,  1)
--  (27, 3,  0)
--  (28, 3, -1)
--  (29, 3, -2)
-- @
--
unwrappedSpiralExample :: ()
unwrappedSpiralExample = ()

-- | Point examples
examples :: [Pt]
examples = Pt <$>
  [ (0, 0)
  , (1, 0)
  , (1, -1)
  , (0, -1)
  , (-1, -1)
  , (-1, 0)
  , (-1, 1)
  , (0, 1)
  , (1, 1)
  , (2, 1)
  , (2, 0)
  , (2, -1)
  , (2, -2)
  , (1, -2)
  , (0, -2)
  , (-1, -2)
  , (-2, -2)
  , (-2, -1)
  , (-2, 0)
  , (-2, 1)
  , (-2, 2)
  , (-1, 2)
  , (0, 2)
  , (1, 3)
  , (2, 2)
  , (3, 2)
  , (3, 1)
  , (3, 0)
  , (3, -1)
  , (3, -2)
  ]

examples2 :: [Pt]
examples2 = Pt <$>
  [ (1, 0)
  , (2, 1)
  , (1, 0)
  , (3, -1)
  , (1, 0)
  , (1, 1)
  , (3, 2)
  , (1, 1)
  , (1, 0)
  , (1, -1)
  , (5, -2)
  , (1, -1)
  , (1, 0)
  , (1, 1)
  , (1, 2)
  , (6, 3)
  , (2, 2)
  , (1, 0)
  , (1, -1)
  , (1, -2)
  , (7, -3)
  , (1, -2)
  , (1, -1)
  , (1, 0)
  , (1, 1)
  , (1, 2)
  , (1, 3)
  , (9, 4)
  , (1, 3)
  , (1, 2)
  , (1, 1)
  , (1, 0)
  , (1, -1)
  , (1, -2)
  , (1, -3)
  , (9, -4)
  , (1, -3)
  , (1, -2)
  , (1, -1)
  , (1, 0)
  , (1, 1)
  , (1, 2)
  , (1, 3)
  , (1, 4)
  , (10, 5)
  , (1, 4)
  , (1, 3)
  , (1, 2)
  , (1, 1)
  , (1, 0)
  , (1, -1)
  , (1, -2)
  , (1, -3)
  , (1, -4)
  , (1, -5)
  ]


-- | Point simple algebraic relations
--
-- @
--  [begin, end] (begin, diff)
--  mult: val
--
--  [0..0] (0, 0)
--  2: 1
--  [0..0] (0, 0)
--  3: -1
--  [0..1] (0, 1)
--  4: 2
--  [1..-1] (1, -2)
--  5: -2
--  [-1..2] (-1, 3)
--  6: 3
--  [2..-2] (2, -4)
--  7: -3
--  [-2..3] (-2, 5)
--  9: 4
--  [3..-3] (3, -6)
--  9: -4
--  [-3..4] (-3, 7)
--  10: 5
--  [4..-4] (4, -8)
-- @
--
ptSimpleAlgRelations :: ()
ptSimpleAlgRelations = ()

-- | From `ptSimpleAlgRelations`
simpleAlgBegins :: [(Integer, Integer)]
simpleAlgBegins =
  [ (0,  0)
  , (0, -0)
  , (1, -1)
  , (2, -2)
  , (3, -3)
  , (4,  4)
  ]

simpleAlgDiffs :: [Integer]
simpleAlgDiffs =
  [  0
  , -0
  ,  1
  , -2
  ,  3
  , -4
  ,  5
  , -6
  ,  7
  , -8
  ]

simpleAlgMults :: [Integer]
simpleAlgMults =
  [ 2
  , 3
  , 4
  , 5
  , 6
  , 7
  , 8
  , 9
  , 10
  ]

-- | `ptSimpleAlgRelations` values:
--
-- @
--   1 -1
--   2 -2
--   3 -3
--   4 -4
--   5
-- @
--
simpleAlgVals :: ()
simpleAlgVals = ()


-- λ> pairList 1 2
-- [1,2]
pairList :: a -> a -> [a]
pairList = (. return) . (:)

-- | `zipWith` on the tail of a list
--
-- @
--  λ> zipWithTail pairList [1..5]
--  [[2,1],[3,2],[4,3],[5,4]]
-- @
--
zipWithTail :: (a -> a -> b) -> [a] -> [b]
zipWithTail = (drop 1 >>=) . zipWith

-- | `zip` on the tail of a list
--
-- @
--  λ> zipTail [1..5]
--  [(2,1),(3,2),(4,3),(5,4)]
-- @
--
zipTail :: [a] -> [(a, a)]
zipTail = drop 1 >>= zip

-- | Successive (first) differences
--
-- @
--  λ> diffs [1..5]
--  [1,1,1,1]
-- @
--
diffs :: Num a => [a] -> [a]
diffs = zipWithTail (-)

