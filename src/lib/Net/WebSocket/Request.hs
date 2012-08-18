module Net.WebSocket.Request
       (readRequest) where

import qualified System.IO as SI

data Request = Request {firstLine :: String, header :: [(String,String)]}

instance Show Request where
  show (Request fl hdr) = "-- START OF REQUEST --\n" ++ fl ++ "\n" ++ hdrString hdr where
                       hdrString [] = "-- END OF REQUEST --\n"
		       hdrString (('\r':[],v):ls) = hdrString ls
		       hdrString ((k,v):ls) = "<" ++ k ++ " -> " ++ v ++ ">\n" ++ hdrString ls

readRequest :: SI.Handle -> IO Request
readRequest handle = do
	fl <- SI.hGetLine handle
	hdr <- readHeader handle
	return Request {header=hdr, firstLine=fl}


readHeader :: SI.Handle -> IO [(String, String)]
readHeader h = do
  rawLine <- SI.hGetLine h
  let hdr = takeWhile (/= ':') rawLine
  let val = takeWhile (/= '\r') $ drop (length hdr + 2) rawLine
  rest <- if hdr /= "\r" then readHeader h else return []
  return ((hdr,val):rest)
 
getHeaderValue :: String -> [String] -> String
getHeaderValue _ [] = []               
getHeaderValue h (s:ss) = if rightHeader then extractKey else getHeaderValue h ss where
                             headerField = h ++ ": "
                             lengthOfHeaderField = length headerField
                             extractKey = drop lengthOfHeaderField s
                             rightHeader = (take lengthOfHeaderField s) == headerField

validateFirstLine :: [String] -> Maybe [String]
validateFirstLine (l:ls) = if take 3 l == "GET" then return ls else Nothing

validateRequest :: [String] -> Maybe [String]
validateRequest header = do
  validateFirstLine header
  return header
  
                  
