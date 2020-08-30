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
    Connected cid ->
      do broadcast jsNewObject "Hello" ()
         broadcast jsSetSize "Hello" (64,128)
         broadcast jsSetBackgroundColor "Hello" "blue"
         cs <- getClients
         broadcast jsSetPosition "Hello" (fromIntegral (length cs) * 100, 0, 0)
         broadcast jsSetVisible "Hello" True
    Disconnected cid ->
      io $ print ev

puzzle :: Puzzle
puzzle = Puzzle
  { pieceRoots  = Map.singleton 0 p0
  , pieceOwner  = Map.empty
  , pieceColors = Map.singleton 0 "red"
  }
  where
  p0 = Piece { pOrigin = V2 20 50
             , pRotate = pi / 3
             , pEmpty  = Map.empty
             , pOwn    = []
             }



-------------------------------------------------------------------------------

{-
jsPiece :: WS.Connection -> Piece -> IO ()
jsPiece conn p =
  do let guid = Text.pack ("piece-" ++ show (pId p))
         app f x = f conn guid x
     jsNewObject `app` ()
     jsSetBackgroundColor `app` Text.pack "red"
     jsSetSize `app` (30,70)
     let V2 x y = pOrigin p
     jsSetPosition `app` (x,y,0)
     jsSetRotation `app` toDeg (pRotate p)
     jsSetVisible `app` True

toDeg :: Radians -> Float
toDeg x = 180 * x / pi
-}
--------------------------------------------------------------------------------
