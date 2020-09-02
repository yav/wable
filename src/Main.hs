{-# Language BlockArguments, OverloadedStrings #-}
module Main where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Concurrent
import Control.Exception

import qualified Data.Aeson as JS
import Data.Aeson(object,(.=))

import GUI
import Puzzle
import V2

main :: IO ()
main = newGUI puzzle \ev ->
  case ev of
    Connected cid -> jsPiece Coord { cOrigin = V2 100 0, cRotate = pi/6 } 0
    Disconnected cid ->
      io $ print ev

puzzle :: Puzzle
puzzle = Puzzle
  { pieceRoots  = Map.singleton 0 p0
  , pieceOwner  = Map.empty
  , pieceColors = Map.singleton 0 "red"
  }
  where
  p0 = Piece { pCoord = Coord { cOrigin = V2 20 50, cRotate = pi / 3 }
             , pEmpty  = Map.empty
             , pOwn    = []
             }




-------------------------------------------------------------------------------


jsPiece :: Coord -> Id -> GUIAction Puzzle ()
jsPiece coord pid =
  do s <- getState
     let guid = Text.pack ("piece-" ++ show pid)
         app f a = broadcast f guid a
     jsNewObject `app` ()
     jsSetBackgroundColor `app` Map.findWithDefault "yellow" pid (pieceColors s)
     jsSetSize `app` (30,70)
     let Just p = Map.lookup pid (pieceRoots s)
         coord1 = coord <> pCoord p
     let V2 x y = cOrigin coord1
     jsSetPosition `app` (x,y,0)
     jsSetRotation `app` toDeg (cRotate coord1)
     jsSetVisible `app` True

toDeg :: Radians -> Float
toDeg x = 180 * x / pi

