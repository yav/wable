{-# Language OverloadedStrings, BlockArguments, RecordWildCards,
             NamedFieldPuns #-}
module GUI where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import Control.Exception(SomeException(..),catch)
import Data.IORef(IORef,newIORef,readIORef,modifyIORef',atomicModifyIORef')
import Network.WebSockets(Connection,sendTextData)
import Data.Aeson(ToJSON,(.=))
import qualified Data.Aeson as JS

-- XXX: Deal with events

type GUI            = IORef GUIState
type ClientId       = Int
data GUIState       = GUIState { nextClient :: !ClientId
                               , clinets    :: !(Map ClientId Connection)
                               }
newGUI :: IO GUI
newGUI = newIORef GUIState { nextClient = 0, clinets = Map.empty }

addClient :: GUI -> Connection -> IO ClientId
addClient ref conn = atomicModifyIORef' ref \GUIState { .. } ->
  ( GUIState { nextClient = nextClient + 1
             , clinets    = Map.insert nextClient conn clinets
             }
  , nextClient
  )

removeClient :: GUI -> ClientId -> IO ()
removeClient ref cid = modifyIORef' ref \GUIState { .. } ->
  GUIState { clinets = Map.delete cid clinets, .. }


--------------------------------------------------------------------------------
newtype GUIAction a = GUIAction (GUI -> IO a)

doAction :: GUI -> GUIAction a -> IO a
doAction ref (GUIAction a) = a ref

broadcast :: Command a -> ObjectId -> a -> GUIAction ()
broadcast f oid a =
  GUIAction \ref ->
    do GUIState { clinets } <- readIORef ref
       mapM_ (\c -> doCommand f c oid a) (Map.elems clinets)

--------------------------------------------------------------------------------
type ObjectId  = Text
type Command a = Connection -> ObjectId -> a -> IO ()

doCommand :: Command a -> Command a
doCommand f c oid a =
  f c oid a `catch` \(SomeException e) -> print e -- XXX: proper logging
  -- XXX: handle client disconnected


jsNewObject :: Command ()
jsNewObject conn i _ = jsCall conn "newObject" i

jsSetPosition :: Command (Float,Float,Float)
jsSetPosition conn i (x,y,z) =
  jsCall conn "setPosition" (i, JS.object [ "x" .= x, "y" .= y, "z" .= z ])

jsSetRotation :: Command Float
jsSetRotation conn i r =
  jsCall conn "setRotation" (i, r)

jsSetSize :: Command (Float,Float)
jsSetSize conn i (w,h) =
  jsCall conn "setSize" (i, JS.object [ "width" .= w, "height" .= h ])

jsSetBackgroundColor :: Command Text
jsSetBackgroundColor conn i c = jsCall conn "setBackgroundColor" (i,c)

jsSetBackground :: Command Text
jsSetBackground conn i c = jsCall conn "setBackground" (i,c)

jsSetVisible :: Command Bool
jsSetVisible conn i b = jsCall conn "setVisible" (i,b)


--------------------------------------------------------------------------------
sendJS :: ToJSON a => Connection -> a -> IO ()
sendJS conn msg = sendTextData conn (JS.encode msg)

jsCall :: ToJSON a => Connection -> Text -> a -> IO ()
jsCall conn f a = sendJS conn (f,a)



