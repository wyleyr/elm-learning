import Mouse
import Graphics.Input (Input, input, clickable)
import Color (Color, red, yellow, green)

data State = Red | Yellow | Green

nextState : State -> State
nextState st = case st of
  Red -> Green
  Green -> Yellow
  Yellow -> Red
  
stateInput : Input State
stateInput = input Red

displayState : State -> Element
displayState st = spacer 30 30 |> 
  (case st of
    Red -> color red 
    Yellow -> color yellow 
    Green -> color green)
  . clickable stateInput.handle (nextState st) 

main = lift displayState stateInput.signal