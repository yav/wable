{-# Language OverloadedStrings, BlockArguments, RecordWildCards,
             NamedFieldPuns #-}
module GUI where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import Data.IORef(IORef,newIORef,readIORef,modifyIORef',atomicModifyIORef')
import Control.Exception(SomeException(..),catch)
import Control.Monad(liftM,ap)
import Network.WebSockets(Connection,sendTextData,receiveData)
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
import Data.Aeson(FromJSON,ToJSON,(.=))
import qualified Data.Aeson as JS
import qualified Snap.Http.Server as Snap

type GUI            = IORef GUIState
type ClientId       = Int
data GUIState       = GUIState { nextClient :: !ClientId
                               , clinets    :: !(Map ClientId Connection)
                               }

newGUI :: (Event -> GUIAction ()) -> IO ()
newGUI app =
  do gui <- newIORef GUIState { nextClient = 0, clinets = Map.empty }
     Snap.quickHttpServe $ WS.runWebSocketsSnap \pending ->
       do conn <- WS.acceptRequest pending
          print "Accepted"
          cid <- addClient gui conn
          let event ev = doAction gui (app ev)
          WS.withPingThread conn 30 (pure ())
            do event (Connected cid)
               let loop = recvJS conn \ev -> event ev >> loop
               loop

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
-- XXX: Client state?

--------------------------------------------------------------------------------

data Event = Connected ClientId
           | Disconnected ClientId
             deriving Show

instance FromJSON Event where
  parseJSON _ = fail "Unknown eevent"


--------------------------------------------------------------------------------
newtype GUIAction a = GUIAction (GUI -> IO a)

instance Functor GUIAction where
  fmap = liftM

instance Applicative GUIAction where
  pure a = GUIAction \_ -> pure a
  (<*>)  = ap

instance Monad GUIAction where
  GUIAction m >>= f = GUIAction \r -> m r >>= \a ->
                                      let GUIAction m1 = f a in m1 r

doAction :: GUI -> GUIAction a -> IO a
doAction ref (GUIAction a) = a ref

getClients :: GUIAction [ClientId]
getClients = GUIAction \ref ->
  do GUIState { clinets } <- readIORef ref
     pure (Map.keys clinets)

broadcast :: Command a -> ObjectId -> a -> GUIAction ()
broadcast f oid a =
  GUIAction \ref ->
    do GUIState { clinets } <- readIORef ref
       mapM_ (\c -> doCommand f c oid a) (Map.elems clinets)

io :: IO a -> GUIAction a
io m = GUIAction \_ -> m

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

recvJS :: FromJSON a => Connection -> (a -> IO ()) -> IO ()
recvJS conn k =
  do bs <- receiveData conn
     case JS.decode bs of
       Just a  -> k a
       Nothing -> print bs -- XXX: logging

jsCall :: ToJSON a => Connection -> Text -> a -> IO ()
jsCall conn f a = sendJS conn (f,a)



