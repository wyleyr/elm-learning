import Graphics.Input (Input, input, clickable)

data Box = Box Int Element

activeElement : Input Int
activeElement = input 1

makeBox boxId = asText ("Box " ++ show boxId)
  |> container 40 50 middle
  |> clickable activeElement.handle boxId
  |> Box boxId

box1 = makeBox 1
box2 = makeBox 2 
allBoxes = [box1, box2]

activate : Int -> Box -> Box
activate activeId (Box id el) = if id == activeId 
  then Box id (el |> color red) 
  else Box id el

displayBox : Box -> Element
displayBox (Box id el) = el

scene : Int -> Element
scene activeId = 
  map (displayBox . (activate activeId)) allBoxes |> flow down

main = lift scene activeElement.signal