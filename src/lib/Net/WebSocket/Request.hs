module Net.WebSocket.Request
       (readRequest,Request (Request) ) where

import qualified System.IO as SI
import qualified Control.Monad as CM
import qualified Control.Monad.Trans as CMT
import qualified Control.Monad.Trans.Error as CMTE
import qualified Data.Char as DC


data Request = Request {firstLine :: String, header :: [(String,String)]}

instance Show Request where
  show (Request fl hdr) = "-- START OF REQUEST --\n" ++ fl ++ "\n" ++ hdrString hdr where
                       hdrString [] = "-- END OF REQUEST --\n"
		       hdrString (('\r':[],v):ls) = hdrString ls
		       hdrString ((k,v):ls) = "<" ++ k ++ " -> " ++ v ++ ">\n" ++ hdrString ls


readRequest :: SI.Handle -> IO Request
readRequest handle = do
	request' <- CMTE.runErrorT (readRequest' handle)
	let (request , action) = case request' of 
	             Right r  -> (r,putStrLn "Parsed the request successfully")
		     Left err -> (Request ("BAD REQUEST : " ++ err) [], putStrLn err)
	action
	return request
	


readRequest' :: SI.Handle -> CMTE.ErrorT String IO Request
readRequest' handle = do
	fl <- CMTE.ErrorT (readFirstRequestLine handle)
	hdr <- CMTE.ErrorT (readHeader handle)
	let request = Request {header=hdr, firstLine=fl}
	return  request


readFirstRequestLine :: SI.Handle -> IO (Either String String)
readFirstRequestLine handle = do
                                l <- SI.hGetLine handle
				return $ validateFirstLine l

readHeader :: SI.Handle -> IO (Either String [(String, String)])
readHeader h = do
  headerLines <- readHeaderLines h
  let h = headerLines2values headerLines
  return (validate h)


readHeaderLines :: SI.Handle -> IO [String]
readHeaderLines h = do
	l <- SI.hGetLine h
	if l == "\r" then return [] else do
		                           r <- readHeaderLines h
					   return (l:r)

headerLines2values :: [String] -> [(String, String)]
headerLines2values [] = []
headerLines2values (x:xs) = let hdr = map DC.toUpper $ takeWhile (/= ':') x
                                val = takeWhile (/= '\r') $ drop (length hdr + 2) x
				in (hdr,val):headerLines2values xs

validateFirstLine :: String -> Either String String
validateFirstLine l = if take 3 l == "GET" then Right l else  Left $ "The first line of the request if faulty: " ++ l

getHeaderValue :: Request -> String -> Either String String
getHeaderValue (Request _ []) key = Left $ "Could not find " ++ key                
getHeaderValue (Request _ ((k,v):ss)) key = if rightHeader then Right v else getHeaderValue  (Request "" ss) key where
                             rightHeader = (map DC.toUpper key) == k

validate :: [(String, String)] -> Either String [(String,String)]
validate x = do
	upgrade <- getHeaderValue (Request "" x) "UPGRADE"
	connection <- getHeaderValue (Request "" x) "CONNECTION"
	if connection == "Upgrade" then Right x else Left "Invalid connection"
