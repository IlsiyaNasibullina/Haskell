{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
import CodeWorld

data Dir = RIGHT | LEFT | UP | DOWN | SAME

type Coords = (Double, Double)

data Tile = Floor | Wall | Button DoorColor | Exit | Door DoorColor

data DoorColor = Red | Blue | DarkBlue | LightPink

data State = State Coords [DoorColor]

isFloor :: (Coords -> Tile) -> State -> Tile
isFloor tilemap (State (x, y) buttons)
  | isDoor (tilemap(x, y)) && oneOf(doorsColor (tilemap (x, y))) buttons = Floor
  | otherwise = tilemap (x, y)

initial :: State
initial = State ((-9), 9) []

handleMap :: Event -> State -> State
handleMap (KeyPress "Up") (State (x, y) press) 
                            = State (newPosition (State (x, y) press) UP) press
handleMap (KeyPress "Down") (State (x, y) press) 
                          = State (newPosition (State (x, y) press) DOWN) press
handleMap (KeyPress "Left") (State (x, y) press) 
                          = State (newPosition (State (x, y) press) LEFT) press
handleMap (KeyPress "Right") (State (x, y) press) 
                          = State (newPosition (State (x, y) press) RIGHT) press
handleMap (KeyPress "Enter") (State (x, y) press) 
  | isButton tile && present == False 
                            = State (x, y) (addColor press (buttonColor tile))
  | isButton tile && present = State (x, y)(removeColor press(buttonColor tile))
  | otherwise = State (x, y) press
  where 
    tile = tileMap (x, y)
    present = oneOf (buttonColor tile) press
handleMap _ coords = coords

drawMap :: State -> Picture
drawMap (State (x, y) buttons) = translated x y (lettering "\x1F6B6") <>
                              drawLevelMap (openDoors buttons tileMap)

tryMove :: Dir -> Coords -> Coords
tryMove RIGHT (x, y) =  (x + 1, y)
tryMove LEFT (x, y) = (x - 1, y)
tryMove UP (x, y) =  (x, y + 1)
tryMove DOWN (x, y) = (x, y - 1)

isButton :: Tile -> Bool
isButton (Button _ ) = True
isButton _ = False


buttonColor :: Tile -> DoorColor
buttonColor (Button Red ) = Red
buttonColor (Button Blue ) = Blue
buttonColor (Button DarkBlue ) = DarkBlue
buttonColor (Button LightPink ) = LightPink

newPosition :: State -> Dir -> Coords
newPosition (State (x, y) pressed) dir
  | canMove (isFloor tileMap (State (tryMove dir (x, y)) pressed)) 
                                                          = tryMove dir (x, y)
  | otherwise = (x, y)

canMove :: Tile -> Bool
canMove Floor = True
canMove Exit = True
canMove (Button dc) = True
canMove _ = False


solution1 :: IO()
solution1 = activityOf initial handleMap drawMap


floorTile :: Coords -> Picture
floorTile (x, y) = translated x y (colored yellow (solidRectangle 0.9 0.9))

wallTile :: Coords -> Picture
wallTile (x, y) = translated x y (colored black (solidRectangle 0.9 0.9))

exitTile :: Coords -> Picture
exitTile (x, y) = translated x y (colored red (solidRectangle 0.2 0.2) 
                  <> colored black (solidRectangle 0.4 0.4)
                  <> colored red (solidRectangle 0.6 0.6)
                  <> colored black (solidRectangle 0.9 0.9))
                  
buttonTile :: Color -> Coords -> Picture
buttonTile c (x, y) = translated x y ((colored c (solidCircle 0.3))
                     <> (colored yellow (solidRectangle 0.9 0.9)))

doorTile :: Color -> Coords -> Picture
doorTile c (x, y) = translated x y ((colored c (solidCircle 0.3))
                    <> (colored black (solidRectangle 0.9 0.9)))


doorColor :: DoorColor -> Color 
doorColor Red = red
doorColor Blue = (lighter 0.2 blue)
doorColor DarkBlue = (darker 0.1 blue)
doorColor LightPink =  pink


drawTileAt :: Coords -> Tile -> Picture
drawTileAt (x, y) Floor = floorTile (x, y)
drawTileAt (x, y) Wall = wallTile (x, y)
drawTileAt (x, y) Exit = exitTile (x, y)
drawTileAt (x, y) (Button dc) = buttonTile (doorColor dc) (x, y)
drawTileAt (x, y) (Door dc) = doorTile (doorColor dc) (x, y)
     
drawRow ::(Coords -> Tile) -> Double -> (Double, Double) -> Picture
drawRow tiles j (from, to)
  | from > to = blank
  | otherwise = drawTileAt (from, j) (tiles (from, j)) 
                <> drawRow tiles j (from + 1 , to) 
                                              
drawRows :: (Coords -> Tile) -> (Double, Double) -> (Double, Double) -> Picture
drawRows tiles (fromY, toY) (fromX, toX) 
  | fromY > toY = blank
  | otherwise = (drawRow tiles fromY (fromX, toX)) 
                  <> (drawRows tiles (fromY + 1, toY) (fromX, toX))


drawLevelMap :: (Coords -> Tile) -> Picture
drawLevelMap gamemap = (drawRows gamemap ((-10), 10) ((-10), 10))

tileMap :: Coords -> Tile
tileMap (x, y) 
  | y == 10 || y == (-10) = Wall
  | x == (-10) || x == 10 = Wall
  | y == 0 && ((x > (-6) && x < (-2)) || (x > 2 && x < 6)) = Door LightPink
  | (y < 3 && y > (-3) && y /= 0) && x == -y = Button LightPink 
  | x == 0 && ((y > (-6) && y < (-2)) || (y > 2 && y < 6)) = Door DarkBlue
  | (y > 1 && y < 5 && x == -6 + y)  = Button DarkBlue 
  | y > (-5) && y < (-1) && x == 6 + y = Button DarkBlue 
  | x == 0 = Wall
  | y == 0 = Wall
  | y == 8 && (x == (-8) || x == (-7)) || y == 7 && x ==(-8) = Button Blue 
  | y == (-8) && (x == 8 || x == 7) || y == (-7) && x == 8 = Button Blue 
  | (y == 5 && x == (-4)) || (y == 4 && x == (-5)) = Door Blue
  | (y == (-5) && x == 4) || (y == (-4) && x == 5) = Door Blue
  | (y < 9 && y > 4 && x == -13 + y) = Button Red 
  | (y > (-9) && y < (-4) && x == 13 + y) = Button Red 
  | (y < 9 && y > 5 || y > 0 && y < 4) && x == -9 + y = Wall
  | (y > (-9) && y < (-5) || y < 0 && y > (-4)) && x == 9 + y = Wall
  | (y == 8 || y == 2) && x > 3 && x < 7 = Door Red
  | (y == (-8) || y == (-2)) && x < (-3) && x > (-7) = Door Red
  | (x == 2 || x == 8) && y > 3 && y < 7 = Door DarkBlue
  | (x == (-2) || x == (-8)) && y < (-3) && y > (-7) = Door DarkBlue
  | (x == 3 || x == 7 || x == (-3) || x == (-7)) && y == x = Wall
  | (x == 3 || x == 7 || x == (-3) || x == (-7)) && abs(x + y) == 10 = Wall
  | (x == 5 || x == (-5)) && y == x = Exit
  | otherwise = Floor
 
isDoor :: Tile -> Bool
isDoor (Door _) = True
isDoor _ = False

doorsColor :: Tile -> DoorColor
doorsColor (Door Red ) = Red
doorsColor (Door Blue ) = Blue
doorsColor (Door LightPink ) = LightPink
doorsColor (Door DarkBlue ) = DarkBlue


  
openDoors :: [DoorColor] -> (Coords -> Tile) -> (Coords -> Tile)
openDoors colors tilemap (x, y)
  | isDoor (tilemap (x, y)) && oneOf(doorsColor (tilemap (x, y))) colors = Floor
  | otherwise = tilemap(x, y)

eqDoorColor :: DoorColor -> DoorColor -> Bool
eqDoorColor Red Red = True
eqDoorColor Blue Blue = True
eqDoorColor DarkBlue DarkBlue = True
eqDoorColor LightPink LightPink = True
eqDoorColor _ _ = False

oneOf :: DoorColor -> [DoorColor] -> Bool
oneOf color [] = False
oneOf color (x:xs) 
  | eqDoorColor color x == True = True
  | otherwise = oneOf color xs
  
  
newTileMap :: Picture
newTileMap = drawLevelMap (openDoors [Red, Blue, DarkBlue, LightPink] tileMap)

solution3 :: IO()
solution3 = drawingOf newTileMap

addColor :: [DoorColor] -> DoorColor -> [DoorColor]
addColor colors color = colors ++ [color]

removeColor :: [DoorColor] -> DoorColor -> [DoorColor]
removeColor [] color = []
removeColor (y:ys) color
  | eqDoorColor color y    = removeColor  ys color
  | otherwise = y : removeColor  ys color


main :: IO()
main = solution1

