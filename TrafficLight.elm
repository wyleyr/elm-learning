import Mouse
import Graphics.Input (Input, input, clickable)
import Color (Color, red, yellow, green, black)

data State = Red | Yellow | Green

nextState : State -> State
nextState st = case st of
  Red -> Green
  Green -> Yellow
  Yellow -> Red
  
stateInput : Input State
stateInput = input Red

lightPosition : State -> Position
lightPosition st = case st of 
  Red -> midTop
  Yellow -> middle
  Green -> midBottom
  
displayLight : State -> Element
displayLight st = spacer 30 30 |> 
  case st of
    Red -> color red 
    Yellow -> color yellow 
    Green -> color green  

trafficLight : State -> Element
trafficLight st = container 30 90 (lightPosition st) (displayLight st) 
  |> color black
  |> clickable stateInput.handle (nextState st)

main = lift trafficLight stateInput.signal