{-# Language BlockArguments, OverloadedStrings #-}
module Main where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Concurrent
import Control.Exception

import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
import qualified Snap.Http.Server as Snap
import qualified Data.Aeson as JS
import Data.Aeson(object,(.=))

import GUI
import Puzzle
import V2

main :: IO ()
main =
  do s <- newGUI
     Snap.quickHttpServe (WS.runWebSocketsSnap (server s))

server :: GUI -> WS.ServerApp
server srv pending =
  do conn <- WS.acceptRequest pending
     print "Accepted"
     WS.withPingThread conn 30 (pure ())
       do jsPiece conn Piece { pId = 1
                             , pOrigin = V2 50 100
                             , pRotate = pi/3
                             , pEmpty = Map.empty
                             }



-------------------------------------------------------------------------------


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

--------------------------------------------------------------------------------
