{-# Language BlockArguments, OverloadedStrings #-}
module Main where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Monad(forM_)
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
    Connected cid -> jsRootPiece 0
    Disconnected cid ->
      io $ print ev

puzzle :: Puzzle
puzzle = Puzzle
  { pieceRoots  = Map.singleton 0 p0
  , pieceOwner  = Map.empty
  , pieceColors = Map.fromList [ (0,"red"), (1,"blue") ]
  }
  where
  p0 = Piece { pCoord = Coord { cOrigin = V2 50 70, cRotate = -pi/3 }
             , pEmpty  = Map.empty
             , pOwn    = [ (V2 64 0, 1) ]
             }




-------------------------------------------------------------------------------


jsRootPiece :: Id -> GUIAction Puzzle ()
jsRootPiece pid =
  do s <- getState
     case Map.lookup pid (pieceRoots s) of
       Just p ->
         do let c = pCoord p
            jsPiece c pid
            forM_ (pOwn p) \(v,i) -> jsPiece c { cOrigin = relative c v } i
       _ -> pure ()

jsPiece :: Coord -> Id -> GUIAction Puzzle ()
jsPiece coord pid =
  do s <- getState
     let guid = Text.pack ("piece-" ++ show pid)
         app f a = broadcast f guid a
     jsNewObject `app` ()
     jsSetBackgroundColor `app` Map.findWithDefault "yellow" pid (pieceColors s)
     jsSetSize `app` (64,64)
     let V2 x y = cOrigin coord
     jsSetPosition `app` (x,y,0)
     jsSetRotation `app` toDeg (cRotate coord)
     jsSetVisible `app` True

toDeg :: Radians -> Float
toDeg x = 180 * x / pi

