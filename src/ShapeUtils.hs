module ShapeUtils where

import Types

cutPointShape :: Shape -> Point -> [Shape]
cutPointShape parent (Point x y) =
  let dx = x - rX parent
      dy = y - rY parent
      shape0 = Rectangle (rX parent) (rY parent) dx dy
      shape1 = Rectangle (rX parent + dx) (rY parent) (rWidth parent - dx) dy
      shape2 = Rectangle (rX parent + dx) (rY parent + dy) (rWidth parent - dx) (rHeight parent - dy)
      shape3 = Rectangle (rX parent) (rY parent + dy) dx (rHeight parent - dy)
  in  [shape0, shape1, shape2, shape3]

cutHorizontalShape :: Shape -> Coordinate -> [Shape]
cutHorizontalShape parent y =
  let dy = y - rY parent
      shape0 = Rectangle (rX parent) (rY parent) (rWidth parent) dy
      shape1 = Rectangle (rX parent) (rY parent + dy) (rWidth parent) (rHeight parent - dy)
  in  [shape0, shape1]

cutVerticalShape :: Shape -> Coordinate -> [Shape]
cutVerticalShape parent x =
  let dx = x - rX parent
      shape0 = Rectangle (rX parent) (rY parent) dx (rHeight parent)
      shape1 = Rectangle (rX parent + dx) (rY parent) (rWidth parent - dx) (rHeight parent)
  in [shape0, shape1]

cutLineShape :: Shape -> Orientation -> Coordinate -> [Shape]
cutLineShape parent Horizontal y = cutHorizontalShape parent y
cutLineShape parent Vertical x = cutVerticalShape parent x

-- | This assumes that shapes are next to each other, and of compatible sizes
mergeShapesHorizontal :: Shape -> Shape -> Shape
mergeShapesHorizontal left right =
    Rectangle (rX left) (rY left) (rWidth left + rWidth right) (rHeight left)

-- | This assumes that shapes are next to each other, and of compatible sizes
mergeShapesVertical :: Shape -> Shape -> Shape
mergeShapesVertical bottom top =
    Rectangle (rX bottom) (rY bottom) (rWidth bottom) (rHeight bottom + rHeight top)

shapeContainsPoint :: Shape -> Point -> Bool
shapeContainsPoint block (Point x y) =
  (x >= rX block) && (x < rX block + rWidth block) &&
    (y >= rY block) && (y < rY block + rHeight block)

