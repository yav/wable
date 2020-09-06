{-# Language BlockArguments, OverloadedStrings #-}
module BasicActions where

import Text.Read(readMaybe)
import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Monad(forM_)

import V2
import GUI
import Coord
import Puzzle
import AppState

toOID :: PieceId -> ObjectId
toOID i = Text.pack ("piece-" ++ show i)

fromOID :: ObjectId -> Maybe PieceId
fromOID xs = case break (== '-') (Text.unpack xs) of
               ("piece",_:xs) -> readMaybe xs
               _ -> Nothing


jsRootPiece :: PieceId -> GUIAction AppState ()
jsRootPiece pid =
  do s <- appPuzzle <$> getState
     case Map.lookup pid (pieceRoots s) of
       Just p ->
         do let c = pCoord p
            jsPiece c pid
            forM_ (pOwn p) \(v,i) -> jsPiece c { cOrigin = relative c v } i
       _ -> pure ()

jsPiece :: Coord -> PieceId -> GUIAction AppState ()
jsPiece coord pid =
  do s <- appPuzzle <$> getState
     let guid = Text.pack ("piece-" ++ show pid)
         app f a = broadcast f guid a
     jsNewObject `app` ()
     jsSetBackgroundColor `app` Map.findWithDefault "yellow" pid (pieceColors s)
     jsSetSize `app` (64,64)
     let V2 x y = cOrigin coord
     jsSetPosition `app` (x,y,0)
     jsSetRotation `app` toDeg (cRotate coord)
     jsSetClickable `app` ()
     jsSetClipPath `app` "M0.5,1 C 0.5,1,0,0.7,0,0.3 A 0.25,0.25,1,1,1,0.5,0.3 A 0.25,0.25,1,1,1,1,0.3 C 1,0.7,0.5,1,0.5,1 Z"
     jsSetVisible `app` True

toDeg :: Radians -> Float
toDeg x = 180 * x / pi





