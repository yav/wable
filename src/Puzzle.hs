{-# Language BlockArguments, NamedFieldPuns, RecordWildCards #-}
module Puzzle where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import Data.Maybe(fromMaybe,listToMaybe)
import Control.Monad(guard)

import V2
import Coord


type PieceId = Int

data Puzzle = Puzzle
  { pieceRoots  :: Map PieceId Piece
  , pieceOwner  :: Map PieceId PieceId  -- ^ This piece s contained in another
  , pieceColors :: Map PieceId Text
  }

data Piece = Piece
  { pCoord      :: Coord

    -- Neighbours are relative to our coordinate system
  , pEmpty      :: Map PieceId V2 -- ^ Available locations of neighbours
  , pOwn        :: [(V2,PieceId)]
  }

movePiece :: PieceId -> V2 -> Puzzle -> Puzzle
movePiece pid loc Puzzle { pieceOwner, pieceRoots, .. } =
  Puzzle { pieceRoots = Map.adjust doMove ownerid pieceRoots, .. }
  where
  ownerid = Map.findWithDefault pid pid pieceOwner
  doMove Piece { pCoord, pOwn, .. } =
    let newLoc = fromMaybe loc
                $ listToMaybe [ loc `sub` r | (r,i) <- pOwn, i == pid ]
    in Piece { pCoord = pCoord { cOrigin = newLoc }, .. }

joinPieces :: PieceId -> PieceId -> Puzzle -> Puzzle
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



