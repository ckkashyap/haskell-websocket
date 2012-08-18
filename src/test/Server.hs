import Net.WebSocket
import qualified Network.Socket as NS
import qualified Control.Concurrent as CC
import qualified System.IO as SI


type HandlerFunc = SI.Handle -> IO ()

main = serveLog "9090" websocket

serveLog :: String              -- ^ Port number or name; 514 is default
         -> HandlerFunc         -- ^ Function to handle incoming messages
         -> IO ()
serveLog port handlerfunc = NS.withSocketsDo $
    do 
       addrinfos <- NS.getAddrInfo 
                    (Just (NS.defaultHints {NS.addrFlags = [NS.AI_PASSIVE]}))
                    Nothing (Just port)
       let serveraddr = head addrinfos

       sock <- NS.socket (NS.addrFamily serveraddr) NS.Stream NS.defaultProtocol

       NS.setSocketOption sock NS.ReuseAddr 1
       NS.bindSocket sock (NS.addrAddress serveraddr)

       NS.listen sock 5

       lock <- CC.newMVar ()

       procRequests lock sock

    where
          -- | Process incoming connection requests
          procRequests :: CC.MVar () -> NS.Socket -> IO ()
          procRequests lock mastersock = 
              do (connsock, clientaddr) <- NS.accept mastersock
                 CC.forkIO $ procMessages lock connsock clientaddr
                 procRequests lock mastersock

          -- | Process incoming messages
          procMessages :: CC.MVar () -> NS.Socket -> NS.SockAddr -> IO ()
          procMessages lock connsock clientaddr =
              do connhdl <- NS.socketToHandle connsock SI.ReadWriteMode
                 SI.hSetBuffering connhdl SI.LineBuffering
                 handlerfunc connhdl
