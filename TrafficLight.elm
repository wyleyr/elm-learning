import Mouse
import Graphics.Input (Input, input, clickable)


data State = Red | Yellow | Green

nextState : State -> State
nextState st = case st of
  Red -> Green
  Green -> Yellow
  Yellow -> Red
  
stateInput : Input State
stateInput = input Red

displayState : State -> Element
displayState st = clickable stateInput.handle (nextState st) <| case st of
  Red -> plainText "The light is red" 
  Yellow -> plainText "The light is yellow" 
  Green -> plainText "The light is green"

main = lift displayState stateInput.signal