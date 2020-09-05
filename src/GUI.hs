{-# Language OverloadedStrings, BlockArguments, RecordWildCards,
             NamedFieldPuns, FlexibleContexts, FlexibleInstances #-}
module GUI where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import qualified Data.Text as Text
import Data.IORef(IORef,newIORef,readIORef,modifyIORef',atomicModifyIORef')
import Control.Exception(SomeException(..),catch)
import Control.Monad(liftM,ap,msum)
import Network.WebSockets(Connection,sendTextData,receiveData)
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
import Data.Aeson(FromJSON,ToJSON,(.=),(.:))
import qualified Data.Aeson as JS
import qualified Snap.Http.Server as Snap

type GUI s          = IORef (GUIState s)
type ClientId       = Int
data GUIState s     = GUIState { nextClient :: !ClientId
                               , clinets    :: !(Map ClientId Connection)
                               , state      :: !s
                               }

newGUI :: s -> (Event -> GUIAction s ()) -> IO ()
newGUI s app =
  do gui <- newIORef GUIState { nextClient = 0, clinets = Map.empty, state = s }
     Snap.quickHttpServe $ WS.runWebSocketsSnap \pending ->
       do conn <- WS.acceptRequest pending
          print "Accepted"
          cid <- addClient gui conn
          let event ev = doAction gui (app ev)
          WS.withPingThread conn 30 (pure ())
            do event (Connected cid)
               let loop = recvJS cid conn \ev -> event ev >> loop
               loop `catch` \ex -> case ex :: WS.ConnectionException of
                                     _ -> do removeClient gui cid
                                             event (Disconnected cid)

addClient :: GUI s -> Connection -> IO ClientId
addClient ref conn = atomicModifyIORef' ref \GUIState { .. } ->
  ( GUIState { nextClient = nextClient + 1
             , clinets    = Map.insert nextClient conn clinets
             , ..
             }
  , nextClient
  )

removeClient :: GUI s -> ClientId -> IO ()
removeClient ref cid = modifyIORef' ref \GUIState { .. } ->
  GUIState { clinets = Map.delete cid clinets, .. }
-- XXX: Client state?

--------------------------------------------------------------------------------

data Event = Connected ClientId
           | Disconnected ClientId
           | Click ClientId ObjectId V2
             deriving Show

instance FromJSON (ClientId -> Event) where
  parseJSON = JS.withObject "event" \o ->
    do ev <- o .: "event"
       case Map.lookup ev evMap of
         Just f -> f o
         _ -> fail ("Unknown event: " ++ Text.unpack ev)

    where
    evMap = Map.fromList
      [ ("click", \o -> do i <- o .: "id"
                           x <- o .: "x"
                           x <- o .: "y"
                           pure \cid -> Click cid i (V2 x y))
      ]


--------------------------------------------------------------------------------
newtype GUIAction s a = GUIAction (GUI s -> IO a)

instance Functor (GUIAction s) where
  fmap = liftM

instance Applicative (GUIAction s) where
  pure a = GUIAction \_ -> pure a
  (<*>)  = ap

instance Monad (GUIAction s) where
  GUIAction m >>= f = GUIAction \r -> m r >>= \a ->
                                      let GUIAction m1 = f a in m1 r

doAction :: GUI s -> GUIAction s a -> IO a
doAction ref (GUIAction a) = a ref

getClients :: GUIAction s [ClientId]
getClients = GUIAction \ref ->
  do GUIState { clinets } <- readIORef ref
     pure (Map.keys clinets)

getState :: GUIAction s s
getState = GUIAction \ref -> state <$> readIORef ref

broadcast :: Command a -> ObjectId -> a -> GUIAction s ()
broadcast f oid a =
  GUIAction \ref ->
    do GUIState { clinets } <- readIORef ref
       mapM_ (\c -> doCommand f c oid a) (Map.elems clinets)

io :: IO a -> GUIAction s a
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

jsSetClickable :: Command ()
jsSetClickable conn i x = jsCall conn "setClickable" i



--------------------------------------------------------------------------------
sendJS :: ToJSON a => Connection -> a -> IO ()
sendJS conn msg = sendTextData conn (JS.encode msg)

recvJS :: ClientId -> Connection -> (Event  -> IO ()) -> IO ()
recvJS cid conn k =
  do bs <- receiveData conn
     case JS.decode bs of
       Just a  -> k (a cid)
       Nothing -> print bs -- XXX: logging

jsCall :: ToJSON a => Connection -> Text -> a -> IO ()
jsCall conn f a = sendJS conn (f,a)



