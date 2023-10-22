{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import CodeWorld

type Time = Double

data SystemState = SystemState CarState PedestrianState BicycleState

data CarState = CarState LightState

data PedestrianState = PedestrianState LightState

data BicycleState = BicycleState LightState

data LightState = Green | Yellow | Red | RedYellow

object :: Double -> Double -> Color -> Double -> Double -> Picture
object x y c z w = translated x y (colored c (solidRectangle z w))

asphalt :: Picture
asphalt = object 0 (-4) grey 25 7

grass :: Picture
grass = object 0 (-8) (darker 0.2 green) 25 10

sky :: Picture
sky = object 0 1 (lighter 0.3 blue) 25 18

bikePath :: Picture
bikePath = object (-5.4) (-4) green 2.75 7 


solidLine :: Picture
solidLine = (object (-9) (-4) white 2 0.4) <> (object 8 (-4) white 10 0.4) 
            <> (object (-8) (-4) white 0.4 7) 
            <> (object 3 (-4) white 0.4 7)

curb :: Picture
curb = object 0 (-4) (dark gray) 25 8

crossLine :: Double -> Picture
crossLine x = translated (-0.5) x (colored yellow (solidRectangle 5 1))

crosswalk :: Picture
crosswalk = crossLine (-1.35) <> crossLine (-3.2) <> crossLine (-5) 
            <> crossLine (-6.7)

lightCircle :: Color -> Double -> Double -> Double -> Picture
lightCircle c y z r = translated y z (colored c (solidCircle r))

trafficLightSystem :: Picture
trafficLightSystem = object (-8) 2.3 black 0.2 5.75 
                    <> translated (-8) 6.8 (colored black (rectangle 1 3.3))
                    <> translated (-8) 6.8(colored white (solidRectangle 1 3.3))
                    <> object (-3.5) 1 black 0.2 3
                    <> translated (-3.5) 4 (colored black (rectangle 1.5 3))
                    <> translated (-3.5) 4(colored white (solidRectangle 1.5 3))
                    <> object 2.7 1 black 0.2 3 
                    <> translated 2.7 4 (colored black (rectangle 1.5 3))
                    <> translated 2.7 4 (colored white (solidRectangle 1.5 3))

carTraffic :: Color -> Color -> Color-> Picture
carTraffic u m l = lightCircle l (-8) 5.8 0.4 
                <> lightCircle m (-8) 6.8 0.4 
                <> lightCircle u (-8) 7.8 0.4
                
pedestrianTraffic :: Color-> Color -> Picture
pedestrianTraffic u l = translated (-3.5) 3.25 (lettering "\x1F6B2") 
                        <> lightCircle l (-3.5) 3.25 0.6
                        <> lightCircle u (-3.5) 4.75 0.6
  
bicycleTraffic :: Color-> Color -> Picture 
bicycleTraffic u l = translated 2.7 3.2 (lettering "\x1F6B6") 
                    <> lightCircle l 2.7 3.25 0.6
                    <> lightCircle u 2.7 4.75 0.6
 
 
carTrafficLight :: LightState -> Picture
carTrafficLight Green = carTraffic (darker 0.3 red) (darker 0.3 yellow) green
carTrafficLight Yellow = carTraffic (darker 0.3 red) yellow (darker 0.3 green)
carTrafficLight Red = carTraffic red (darker 0.3 yellow) (darker 0.3 green)
carTrafficLight RedYellow = carTraffic red yellow (darker 0.3 green)
                   

pedestrianTrafficLight :: LightState -> Picture
pedestrianTrafficLight Green = pedestrianTraffic red (darker 0.3 green)
pedestrianTrafficLight Yellow = 
                           pedestrianTraffic (darker 0.3 red) (darker 0.3 green)
pedestrianTrafficLight Red = pedestrianTraffic (darker 0.3 red)  green 
pedestrianTrafficLight RedYellow = pedestrianTraffic (darker 0.3 red) green 

bicycleTrafficLight :: LightState -> Picture
bicycleTrafficLight Green = bicycleTraffic red (darker 0.3 green)
bicycleTrafficLight Yellow = bicycleTraffic (darker 0.3 red) (darker 0.3 green)
bicycleTrafficLight Red = bicycleTraffic (darker 0.3 red)  green 
bicycleTrafficLight RedYellow = bicycleTraffic (darker 0.3 red)  green 

road :: Picture 
road = trafficLightSystem <> crosswalk <> bikePath <> solidLine <> asphalt 
        <> curb <> sky <> grass

getCarLight :: Time -> LightState
getCarLight t 
  | t < 1 = Yellow
  | t < 4 = Red
  | t < 5 = RedYellow
  | t < 8 = Green
  | otherwise = getCarLight (t - 8)
  
getPedestrian :: Time -> LightState
getPedestrian t
  | t < 1 = Green
  | t < 4 = RedYellow
  | t < 4.15 = Red
  | t < 4.3 = Yellow
  | t < 4.45 = Red
  | t < 4.6 = Yellow
  | t < 4.75 = Red 
  | t < 5 = Yellow
  | t < 8 = Green
  | otherwise = getPedestrian (t - 8)

getBicycle :: Time -> LightState
getBicycle t
  | t < 1 = Green
  | t < 4 = RedYellow
  | t < 4.15 = Red
  | t < 4.3 = Yellow
  | t < 4.45 = Red
  | t < 4.6 = Yellow
  | t < 4.75 = Red 
  | t < 5 = Yellow
  | t < 8 = Green
  | otherwise = getBicycle (t - 8)

myPicture :: Time -> Picture 
myPicture t =
  let systemState = SystemState (CarState car) (PedestrianState pedestrian) 
                    (BicycleState bicycle)
      car = getCarLight t
      pedestrian = getPedestrian t
      bicycle = getBicycle t
   in carTrafficLight car <> pedestrianTrafficLight pedestrian 
       <> bicycleTrafficLight bicycle <> road  



main :: IO ()
main = animationOf myPicture
