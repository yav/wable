module Coord where

import V2

data Coord = Coord
  { cOrigin :: V2
  , cRotate :: Radians
  }

instance Semigroup Coord where
  t1 <> t2 = Coord { cOrigin = add (cOrigin t1) (cOrigin t2)
                   , cRotate = cRotate t1 + cRotate t2
                   }

instance Monoid Coord where
  mempty = Coord { cOrigin = V2 0 0, cRotate = 0 }

-- | Turn a relative vector to an absolute one
relative :: Coord -> V2 -> V2
relative c v = cOrigin c `add` rotate (cRotate c) v

-- | Turn an absolute vector to a relative one
absolute :: Coord -> V2 -> V2
absolute c v = rotate (negate (cRotate c)) (v `sub` cOrigin c)

-- | Turn a relative vector in the first coordinate system, into
-- a relative vector in the second one.
changeCoord :: Coord -> Coord -> V2 -> V2
changeCoord old new = absolute new . relative old

