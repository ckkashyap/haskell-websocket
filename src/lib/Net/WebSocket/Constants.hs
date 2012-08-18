module Net.WebSocket.Constants 
       (
          web_socket_guid     
        , opcode_continuation 
        , opcode_text         
        , opcode_binary       
        , opcode_close        
        , opcode_ping         
        , opcode_pong         
       ) where

web_socket_guid     = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
opcode_continuation = 0x00
opcode_text         = 0x01
opcode_binary       = 0x02
opcode_close        = 0x08
opcode_ping         = 0x09
opcode_pong         = 0x0a



