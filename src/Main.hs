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

import V2
import Coord
import GUI
import Puzzle
import AppState
import BasicActions

main :: IO ()
main = newGUI appState \ev ->
  case ev of
    Connected cid ->
      do jsRootPiece 0
         broadcast jsSetClickable "body" ()
    Disconnected cid ->
      io $ print ev
    Click cid oid loc ->
      case fromOID oid of
        Nothing -> pure () -- undefined
        Just i -> pure () -- updateState (pickUp cid i)

appState :: AppState
appState = AppState
  { appPuzzle = puzzle
  , appUsers  = Map.empty
  }

puzzle :: Puzzle
puzzle = Puzzle
  { pieceRoots  = Map.singleton 0 p0
  , pieceOwner  = Map.empty
  , pieceColors = Map.fromList [ (0,"red"), (1,"blue") ]
  }
  where
  p0 = Piece { pCoord  = Coord { cOrigin = V2 50 70, cRotate = -pi/3 }
             , pEmpty  = Map.empty
             , pOwn    = [ (V2 64 0, 1) ]
             }







