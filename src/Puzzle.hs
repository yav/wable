{-# Language BlockArguments #-}
module Puzzle where

import Data.Maybe(fromMaybe)
import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad(guard)

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



data Puzzle = Puzzle
  { pieceRoots  :: Map Id Piece
  , pieceOwner  :: Map Id Id          -- ^ This piece s contained in another
  , pieceColors :: Map Id Text
  }

data Piece = Piece
  { pCoord      :: Coord

    -- Neighbours are relative to our coordinate system
  , pEmpty      :: Map Id V2 -- ^ Available locations of neighbours
  , pOwn        :: [(V2,Id)]
  }

joinPieces :: Id -> Id -> Puzzle -> Puzzle
joinPieces pid1 pid2 puzz =
  fromMaybe puzz
  do p1    <- Map.lookup pid1 (pieceRoots puzz)
     slot1 <- Map.lookup pid2 (pEmpty p1)
     p2    <- Map.lookup pid2 (pieceRoots puzz)
     slot2 <- Map.lookup pid1 (pEmpty p2)

     let o1 = pCoord p1
     let o2 = pCoord p1
     guard (close (cOrigin o2) (relative o1 slot1))
     guard (close (cOrigin o1) (relative o2 slot2))

     let p = Piece { pCoord = o1
                   , pEmpty = Map.union
                                (Map.delete pid2 (pEmpty p1))
                                (changeCoord o2 o1 <$>
                                                Map.delete pid1 (pEmpty p2))
                   , pOwn = (slot1,pid2)
                          : pOwn p1 ++
                            [ (changeCoord o2 o1 v, i) | (v,i) <- pOwn p2 ]
                   }
     pure puzz { pieceRoots = Map.insert pid1 p
                                (Map.delete pid2 (pieceRoots puzz))
               , pieceOwner = foldr (\k -> Map.insert k pid1) (pieceOwner puzz)
                                    (pid2 : map snd (pOwn p2))
               }


close :: V2 -> V2 -> Bool
close v1 v2 = len (sub v1 v2) < 5



