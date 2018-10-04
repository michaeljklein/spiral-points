module Data.CardinalDir where

import Lib

-- | Cardinal directions, ordered as follows:
--
-- @
--       N
--       1
--     8   2
--  W 7      3 E
--     6   4
--       5
--       S
-- @
--
data CardinalDir
  = N
  | NE
  | E
  | SE
  | S
  | SW
  | W
  | NW
  deriving (Eq, Ord, Read, Show, Enum)

-- | Convert a point to its `CardinalDir`, relative to @(0, 0)@
toCardinalDir :: Pt -> CardinalDir
toCardinalDir = toCardinalDirUnsafe . signum

-- | Convert to `CardinalDir`, assuming `Pt` only contains
-- values in @[-1, 0, 1]@.
toCardinalDirUnsafe :: Pt -> CardinalDir
toCardinalDirUnsafe (Pt ( 0,  1)) = N
toCardinalDirUnsafe (Pt ( 1,  1)) = NE
toCardinalDirUnsafe (Pt ( 1,  0)) = E
toCardinalDirUnsafe (Pt ( 1, -1)) = SE
toCardinalDirUnsafe (Pt ( 0, -1)) = S
toCardinalDirUnsafe (Pt (-1, -1)) = SW
toCardinalDirUnsafe (Pt (-1,  0)) = W
toCardinalDirUnsafe (Pt (-1,  1)) = NW

-- | Convert a `CardinalDir` to a `Pt`
fromCardinalDir :: CardinalDir -> Pt
fromCardinalDir N  = Pt ( 0,  1)
fromCardinalDir NE = Pt ( 1,  1)
fromCardinalDir E  = Pt ( 1,  0)
fromCardinalDir SE = Pt ( 1, -1)
fromCardinalDir S  = Pt ( 0, -1)
fromCardinalDir SW = Pt (-1, -1)
fromCardinalDir W  = Pt (-1,  0)
fromCardinalDir NW = Pt (-1,  1)

