module Net.WebSocket (
       websocket ) where

import qualified System.IO as SI
import Net.WebSocket.Constants
import Net.WebSocket.Request

websocket :: SI.Handle -> IO ()
websocket handle = do
	request@(Request fl _ ) <- readRequest handle
	if (take 3 fl)  == "BAD" then putStrLn (show request) >> SI.hClose handle >> return () else do
		putStrLn $ show request
		putStrLn $ show web_socket_guid     
		putStrLn $ show opcode_continuation 
		putStrLn $ show opcode_text         
		putStrLn $ show opcode_binary       
		putStrLn $ show opcode_close        
		putStrLn $ show opcode_ping         
		putStrLn $ show opcode_pong         
		return ()





getResponseHeaders :: String -> String -> [String]
getResponseHeaders protocol key =
  [
      "HTTP/1.1 101 Switching Protocols"
    , "Upgrade: websocket"
    , "Connection: Upgrade"
    , "Sec-WebSocket-Accept: " ++ key
    , "Sec-WebSocket-Protocol: " ++ protocol
    , "Orgin: null"
  ]

