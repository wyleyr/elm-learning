import Graphics.Input (Input, input, button, clickable)

-- Box type, constructor and accessors
data Box = Box Int Element

getId (Box id el) = id

makeBox boxId = asText ("Box " ++ show boxId)
  |> container 40 50 middle
  |> clickable activeElement.handle boxId
  |> Box boxId
  
-- Input representing the active Box
activeElement : Input Int
activeElement = input 1

-- Input representing clicks to the "Add a box" button, i.e., requests for new boxes
newRequest : Input Bool
newRequest = input False

-- Control button to request a new box
newBoxButton : Element
newBoxButton = button newRequest.handle True "Add a box"

-- initial list of boxes
box1 = makeBox 1
box2 = makeBox 2 

allBoxes : [Box]
allBoxes = [box1, box2]

-- add a Box to the end of an existing list, giving it a unique ID
addBox : [Box] -> [Box]
addBox bxs = 
  let maxId = foldl max 0 (map getId bxs)
      newId = maxId + 1
  in bxs ++ [makeBox newId]
    
addBoxOnClick : Bool -> [Box] -> [Box]
addBoxOnClick cl bxs = addBox bxs

-- "past dependent" boxes: a signal that updates to a new list of boxes,
-- based on the previous list, when a request for a new box comes in
pdBoxes : Signal [Box]
pdBoxes = foldp addBoxOnClick allBoxes newRequest.signal

-- combines the signals representing the past-dependent list of boxes and active box ID
selectBox : Int -> [Box] -> (Int, [Box])
selectBox id bxs = (id, bxs)

activeBoxes : Signal (Int, [Box])
activeBoxes = lift2 selectBox activeElement.signal pdBoxes

-- display a scene representing the list of boxes, highlighting the active one
displayBox : Box -> Element
displayBox (Box id el) = el

activate : Int -> Box -> Box
activate activeId (Box id el) = if id == activeId 
  then Box id (el |> color red) 
  else Box id el

scene : (Int, [Box]) -> Element
scene (activeId, bxs) = 
  newBoxButton :: (map (displayBox . (activate activeId)) bxs) |> flow down

main = lift scene activeBoxes