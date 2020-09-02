module Puzzle where

import Data.Text(Text)
import Data.Map(Map)

import V2

type Id = Int

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

data Puzzle = Puzzle
  { pieceRoots  :: Map Id Piece
  , pieceOwner  :: Map Id Id          -- ^ This piece s contained in another
  , pieceColors :: Map Id Text
  }

data Piece = Piece
  { pCoord      :: Coord

    -- Neighbours are relative to our coordinate system
  , pEmpty      :: Map Id V2 -- ^ Available locations of neighbours
  , pOwn        :: [(V2,Piece)]
  }





