{-# Language BlockArguments, NamedFieldPuns, RecordWildCards #-}
module AppState where

import Data.Maybe(fromMaybe,listToMaybe)
import Data.List(find)
import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad(guard)

import V2
import Coord
import GUI
import Puzzle

type UserId  = Int

data AppState = AppState
  { appPuzzle :: Puzzle
  , appUsers  :: Map UserId UserState
  }

data UserState = UserState
  { userName    :: Text
  , userConn    :: ClientId
  , userHolding :: Maybe PieceId
  }

updateUser :: UserId -> (UserState -> UserState) -> AppState -> AppState
updateUser uid f AppState { appUsers, .. } =
                 AppState { appUsers = Map.adjust f uid appUsers, .. }

getUser :: UserId -> (UserState -> Maybe a) -> AppState -> Maybe a
getUser uid f AppState { appUsers } = f =<< Map.lookup uid appUsers

pickUpPiece :: UserId -> PieceId -> AppState -> AppState
pickUpPiece uid pid = updateUser uid \UserState { .. } ->
                                      UserState { userHolding = Just pid, .. }

dropPiece :: UserId -> AppState -> AppState
dropPiece uid = updateUser uid \UserState { .. } ->
                                UserState { userHolding = Nothing, .. }

getHeldPiece :: UserId -> AppState -> Maybe PieceId
getHeldPiece uid = getUser uid userHolding

moveHeld :: UserId -> V2 -> AppState -> AppState
moveHeld uid loc s@AppState { appPuzzle, .. } =
  case getHeldPiece uid s of
    Nothing -> s
    Just pid -> AppState { appPuzzle = movePiece pid loc appPuzzle, .. }

