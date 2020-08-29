module Puzzle where

import Data.Text(Text)
import Data.Map(Map)

import V2

type Id = Int


data Puzzle = Puzzle
  { pieceRoots  :: Map Id Piece
  , pieceOwner  :: Map Id Id          -- ^ This piece s contained in another
  , pieceColors :: Map Id Text
  }

data Piece = Piece
  { pId         :: Id
  , pOrigin     :: V2
  , pRotate     :: Radians

    -- Neighbours are relative to our coordinate system
  , pEmpty      :: Map Id V2 -- ^ Available locations of neighbours
  , pOwn        :: [(V2,Piece)]
  }





