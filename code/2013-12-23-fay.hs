import Prelude hiding (log)
import FFI

data Element

createElement :: String -> Fay Element
createElement = ffi "document.createElement(%1)"

setInnerHTML :: Element -> String -> Fay ()
setInnerHTML = ffi "%1['innerHTML'] = %2"

setClassName :: Element -> String -> Fay ()
setClassName = ffi "%1['innerHTMl'] = %2"

appendChild :: Element -> Element -> Fay ()
appendChild = ffi "%1.appendChild(%2)"

log :: Element -> String -> String -> Fay ()
log container msg cls = do
  logNode <- createElement "li"
  setInnerHTML logNode msg
  setClassName logNode cls
  container `appendChild` logNode

getElementById :: String -> Fay Element
getElementById = ffi "document.getElementById(%1)"

data WebSocket

newWebSocket :: String -> Fay WebSocket
newWebSocket = ffi "new WebSocket(%1)"

class Eventable a

data Event

instance Eventable WebSocket
instance Eventable Element

addEventListener :: Eventable a => a -> String -> (Event -> Fay ()) -> Fay ()
addEventListener = ffi "%1[%2] = %3"

main :: Fay ()
main = do
  prompt <- getElementById "prompt"
  messages <- getElementById "repl"
  connectionToSanta <- newWebSocket "ws://localhost:8080"

  addEventListener connectionToSanta "onopen" $ \_ -> do
    setDisabled prompt False
    addEventListener prompt "onkeydown" $ \e -> do
      if (eventKeyCode e == 13)
        then do
          let message = elementValue prompt
          connectionToSanta `send` message
          log messages message "me"
          clearValue prompt
        else return ()

  addEventListener connectionToSanta "onmessage" $ \e -> do
    log messages (messageData e) ""


setDisabled  :: Element -> Bool -> Fay ()
setDisabled = ffi "%1.disabled = %2"

eventKeyCode :: Event -> Int
eventKeyCode = ffi "%1.keyCode"

clearValue :: Element -> Fay ()
clearValue = ffi "%1.value = ''"

send :: WebSocket -> String -> Fay ()
send = ffi "%1.send(%2)"

elementValue :: Element -> String
elementValue = ffi "%1.value"

messageData :: Event -> String
messageData = ffi "%1.data"
