{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}

import           CodeWorld
import           Data.Text (Text)
-- | Elevator button.
data Button = GoUp | GoDown | STOP
-- | Elevator mode of operation.
data Mode = Idle | MovingUp | MovingDown

data System = System Mode Double


convert :: Text -> Color -> Picture
convert text c = (colored c (lettering text)) 

-- | Render elevator button.
drawButton :: Button -> Picture
drawButton GoUp = colored white (solidPolygon [(0, 0.7), ((-0.5), (-0.5)), 
                                                    (0, (-0.2)), (0.5, (-0.5))])
                  <> colored (light grey) (solidCircle 0.85) 
                  <> colored black (solidCircle 1)
drawButton GoDown = colored white (solidPolygon [(0, (-0.7)), (0.5, 0.5), 
                                                       (0, 0.2), ((-0.5), 0.5)])
                  <> colored (light grey) (solidCircle 0.85) 
                  <> colored black (solidCircle 1)
drawButton STOP = scaled 0.55 0.55 (convert "STOP" red) 
                  <> colored (light grey) (solidCircle 0.85) 
                  <> colored black (solidCircle 1)
   
translate :: Double -> Double -> Picture -> Picture
translate x y pic = translated x y (pic)

-- | Draw several objects
-- some distance apart from each other. 
asSpaced :: Double -- ˆ How far apart to draw objects. 
  -> (a -> Picture) -- ˆ How to draw a single object. 
  -> [a] -- ˆ A list of objects to draw.
  -> Picture
asSpaced _ _ [] = blank
asSpaced x draw (p : ps) =  asSpaced x draw ps <> translate y 0 (pic) 
  where 
    pic = draw p 
    len = fromIntegral (length ps)
    y = x * len

solution1 :: IO()
solution1 = drawingOf (asSpaced 3 drawButton [GoDown, GoUp, STOP])

-- | Render elevator mode.
drawMode :: Mode -> Picture
drawMode Idle = colored grey (solidPolygon [(0, 1.3), ((-0.7), 0.2), 
                                                          (0, 0.5), (0.7, 0.2)])
                <> colored grey (solidPolygon [(0, (-1.3)), ((-0.7), (-0.2)), 
                                                    (0, (-0.5)), (0.7, (-0.2))])
                <> colored (lighter 0.3 grey) (solidRectangle 1.8 2.8) 
                <> colored black (solidRectangle 2 3)
drawMode MovingUp = colored red (solidPolygon [(0, 1.3), ((-0.7), 0.2), 
                                                          (0, 0.5), (0.7, 0.2)])
                <> colored grey (solidPolygon [(0, (-1.3)), ((-0.7), (-0.2)), 
                                                    (0, (-0.5)), (0.7, (-0.2))])
                <> colored (lighter 0.3 grey) (solidRectangle 1.8 2.8) 
                <> colored black (solidRectangle 2 3)           
drawMode MovingDown =  colored grey (solidPolygon [(0, 1.3), ((-0.7), 0.2), 
                                                          (0, 0.5), (0.7, 0.2)])
                <> colored red (solidPolygon [(0, (-1.3)), ((-0.7), (-0.2)), 
                                                    (0, (-0.5)), (0.7, (-0.2))])
                <> colored (lighter 0.3 grey) (solidRectangle 1.8 2.8) 
                <> colored black (solidRectangle 2 3)
 
-- | FSM corresponding to a simple elevator.
elevator :: Mode -> [(Button, Mode)]
elevator Idle = [(GoDown, MovingDown), (GoUp, MovingUp)]
elevator MovingUp = [(STOP, Idle)]
elevator MovingDown = [(STOP, Idle)]

equal :: Button -> Button -> Bool
equal STOP STOP = True
equal GoUp GoUp = True
equal GoDown GoDown = True
equal _ _= False


findTransition :: s -> a -> [(a, s)] -> (a -> a -> Bool) -> s
findTransition state _ [] _ = state
findTransition state butt ((newButt, newState) : xs) eq
  | eq butt newButt = newState
  | otherwise = findTransition state butt xs eq


-- | Apply an action (if any) to the current state 
-- of a finite state machine.
applyAction :: Maybe a -> (a -> a -> Bool) -> (s -> [(a, s)]) -> s -> s
applyAction Nothing _ _ state = state
applyAction (Just a) eq elev state = findTransition state a (elev state) eq 

initial :: Mode
initial = Idle

handleElevator :: Event -> Mode -> Mode
handleElevator (KeyPress "Up") Idle = 
                                  (applyAction (Just GoUp) equal elevator Idle)
handleElevator (KeyPress "Down") Idle = 
                                (applyAction (Just GoDown) equal elevator Idle)
handleElevator (KeyPress " ") MovingUp = 
                              (applyAction (Just STOP) equal elevator MovingUp)
handleElevator (KeyPress " ") MovingDown = 
                            (applyAction (Just STOP) equal elevator MovingDown)
handleElevator _ mode = (applyAction Nothing equal elevator mode)


solution2 :: IO()
solution2 = activityOf initial handleElevator drawMode


converting :: Event -> Maybe Button
converting (KeyPress "Up") = Just GoUp
converting (KeyPress "Down") = Just GoDown
converting (KeyPress " ") = Just STOP
converting _ = Nothing


listOfButtons :: [(a, s)] -> [a]
listOfButtons [] = []
listOfButtons ((buttons, _) : xs) = buttons : listOfButtons xs

-- a = button,  s = mode

joint :: s -> (s -> [(a, s)]) -> (s -> Picture) -> (a -> Picture) -> Picture
joint state buttons drawM drawB = drawM state 
                                  <> translate 3 0 (asSpaced 3 drawB arr)
  where 
    arr = listOfButtons (buttons state)

-- | Interactive finite state machine simulation.
interactiveFSM
  :: s
  -> (a -> a -> Bool)
  -> (s -> [(a, s)])
  -> (Event -> Maybe a)
  -> (s -> Picture)
  -> (a -> Picture)
  -> IO ()
interactiveFSM state eq elev conv drawSt drawBut = activityOf state handle draw
  where 
    draw st = (joint st elev drawSt drawBut)
    handle event st = applyAction (conv event) eq elev st

solution3 :: IO ()
solution3 = interactiveFSM initial equal elevator converting drawMode drawButton





drawSystem :: System -> Picture
drawSystem (System _ x) = translated (-6) x
             (translated 0 (-2) (scaled 5.5 5.5 (lettering "\x1F6B6")) 
             <> colored white (solidRectangle 5.2 9.2)
             <> colored black (solidRectangle 6 10))

evolvingTime ::  Double -> Mode -> System -> System
evolvingTime t MovingUp (System mode x) = (System mode (x + t))
evolvingTime t MovingDown (System mode x) = (System mode (x - t))
evolvingTime _ _ sys = sys

interactiveSystem :: s -- ˆ Initial state of a FSM.
  -> (a -> a -> Bool) -- ˆ FSM action equality test.
  -> (s -> [(a, s)]) -- ˆ FSM State transitions.
  -> (Event -> Maybe a) -- ˆ How to convert events into actions.
  -> (s -> Picture) -- ˆ How to draw states.
  -> (a -> Picture) -- ˆ How to draw actions.
  -> system  -- ˆ System state, whose modes are modelled with FSM
  -> (Double -> s -> system -> system) -- ˆ How system evolves with time.
  -> (system -> Picture) -- ˆ How to render system.
  -> IO ()
interactiveSystem st eq elev conv drawM drawB sys evolv drawS 
                                              = activityOf (st, sys) handle draw
  where 
    draw (buttons, system) = (drawS system) 
                                          <> (joint buttons elev drawM drawB)
    handle (TimePassing t) (but, system) = (but, evolv t but system)
    handle event (but, system) = (applyAction (conv event) eq elev but, system)


solution4 :: IO()
solution4 = interactiveSystem initial equal elevator converting 
                  drawMode drawButton (System Idle 0.0) evolvingTime drawSystem

main :: IO()
main = solution4
