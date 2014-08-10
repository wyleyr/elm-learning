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
displayState st = case st of
  Red -> plainText "The light is red" |> clickable stateInput.handle (nextState st)
  Yellow -> plainText "The light is yellow" |> clickable stateInput.handle (nextState st)
  Green -> plainText "The light is green" |> clickable stateInput.handle (nextState st)

main = lift displayState stateInput.signal