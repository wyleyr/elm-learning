import Graphics.Input (Input, input, button, clickable)
import Graphics.Input.Field (Field, field, Content, noContent, Selection, Forward, defaultStyle)

-- State representation
data State = State Int [Box]

data Event = NewID Int | NewBox Bool | NewContent Content

-- Box type, constructor and accessors
data Box = Box Int String

getId (Box i s) = i
getLabel (Box i s) = s
  
-- Input representing the active Box
activeElement : Input Int
activeElement = input 1

-- Input representing clicks to the "Add a box" button, i.e., requests for new boxes
newRequest : Input Bool
newRequest = input False

-- Control button to request a new box
newBoxButton : Element
newBoxButton = button newRequest.handle True "Add a box"

-- Input representing labels entered by user
newLabel : Input Content
newLabel = input noContent

-- Field in which user enters labels
labelOfActive : State -> Content
labelOfActive (State activeId bxs) = 
  let 
    isActive (Box id _) = activeId == id
    activeBoxes = filter isActive bxs
    activeLabel = case activeBoxes of
      [] -> noContent
      (Box _ lbl)::others -> Content lbl (Selection (String.length lbl) (String.length lbl) Forward)
   in activeLabel

activeBoxLabel : Signal Content
activeBoxLabel = lift labelOfActive states

labelField : Signal Element
labelField = field defaultStyle newLabel.handle id "New Label" <~ activeBoxLabel


-- initial list of boxes
box1 = Box 1 "Box 1"
box2 = Box 2 "Box 2"

initialBoxes : [Box]
initialBoxes = [box1, box2]

boxes : Input [Box]
boxes = input initialBoxes

-- add a Box to the end of an existing list, giving it a unique ID
addBox : [Box] -> [Box]
addBox bxs = 
  let maxId = foldl max 0 (map getId bxs)
      newId = maxId + 1
  in bxs ++ [Box newId ("Box " ++ show newId)]

-- relabel a selected box within an existing list
relabel : Content -> Box -> Box
relabel content (Box id lb) = Box id content.string

relabelWith : Content -> Int -> [Box] -> [Box]
relabelWith lbl activeId bxs = let 
    isActive (Box id lbl) = activeId == id
    maybeRelabel bx = if isActive bx then relabel lbl bx else bx
    newBxs = map maybeRelabel bxs
  in newBxs

-- combine the various input signals into a single signal of Events 
events : Signal Event
events = merges [ NewID <~ activeElement.signal
                , NewBox <~ newRequest.signal 
                , NewContent <~ newLabel.signal
                ]

-- maintain global state, updating on the basis of Events 
evolve : Event -> State -> State
evolve ev (State activeId bxs) = let
    newId = case ev of (NewID id) -> id 
                       _ -> activeId
    newBxs = case ev of
      (NewBox True) -> addBox bxs
      (NewContent label) -> relabelWith label newId bxs
      _ -> bxs
  in State newId newBxs
  
states : Signal State
states = foldp evolve (State 1 initialBoxes) events

-- display a scene representing the list of boxes, highlighting the active one
displayBox : Box -> Element
displayBox (Box id lbl) = plainText lbl 
  |> container 100 50 middle
  |> clickable activeElement.handle id

displayActiveBox : Box -> Element
displayActiveBox = (color red) . displayBox

displayBoxes : Int -> [Box] -> [Element]
displayBoxes activeId bxs = let 
    isActive (Box id lbl) = activeId == id
    disp bx = if isActive bx then displayActiveBox bx else displayBox bx
  in map disp bxs

scene : State -> Element -> Element
scene (State activeId bxs) lblField = 
  [newBoxButton, lblField] ++ displayBoxes activeId bxs |> flow down


main = lift2 scene states labelField