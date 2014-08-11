data Box =  SmallSquare | BigSquare | LongBox [Box] | TallBox [Box]

displayBox : Box -> Element
displayBox bx = case bx of
  SmallSquare -> spacer 30 30 |> color red
  BigSquare -> spacer 90 90 |> color green
  LongBox bxs -> flow right <| map displayBox bxs
  TallBox bxs -> flow down <| map displayBox bxs
  
theBoxes = TallBox [ SmallSquare
                   , BigSquare
                   , LongBox [SmallSquare, BigSquare]
                   , LongBox [SmallSquare, TallBox [SmallSquare, SmallSquare, SmallSquare]]
                   ]
                   
main = displayBox theBoxes