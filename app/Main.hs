{-# NegativeLiterals #-}

module Main (main) where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Simulate


windowSize :: Int
windowSize = 900

displayWindow :: Display
displayWindow = InWindow "Kmeans" (windowSize, windowSize) (300, 100)

animationFunc :: Float -> Picture
animationFunc time = Circle (2 * time)

main = simulate
  displayWindow
  white
  simulationRate
  initialModel
  draw
  update
  where
    simulationRate :: Int
    simulationRate = 20

    initialModel :: Model
    initialModel = Model {points = sample1, centroids = []}


data Model = Model { 
    points :: [(Float, Float)], 
    centroids :: [(Float, Float)]
}

draw :: Model -> Picture
draw m = pictures $ [drawAxes, drawPoints] <*> pure m

update :: ViewPort -> Float -> Model -> Model
update _ dt m = m


drawAxes :: p -> Picture
drawAxes _ = pictures $ [line [(negate maxVal, 0), (maxVal, 0)], line [(0, negate maxVal), (0, maxVal)] ]
             where
                maxVal = (fromIntegral windowSize) / 2


drawPoints (Model ps _) = pictures $ map drawPointAt ps


drawPointAt :: (Float, Float) -> Picture
drawPointAt (x,y) = translate x y (circle 5)

sample1 = [(100.5, -200.3), (150.8, -120.7), (-250.0, 80.0), (-200.0, -250.0), (150.0, 200.0), (-220.0, -300.0), (180.0, -150.0), (-180.0, -280.0), (220.0, 220.0), (-250.0, 180.0), (-210.0, 120.0), (100.0, -80.0), (230.0, 130.0), (-200.0, 50.0), (-170.0, -200.0), (-120.0, -180.0), (160.0, -110.0), (-110.0, 100.0), (230.0, -120.0), (190.0, -130.0), (300.0, -50.0), (230.0, -180.0), (-160.0, 220.0), (170.0, 130.0), (-280.0, -250.0), (-220.0, -180.0), (150.0, 250.0), (-170.0, -160.0), (-200.0, 300.0), (140.0, -160.0), (210.0, -120.0), (-170.0, 220.0), (-150.0, 170.0), (210.0, -170.0), (-190.0, -200.0), (-230.0, 220.0), (160.0, -130.0), (120.0, -180.0), (280.0, -50.0), (240.0, -80.0), (-260.0, 200.0), (220.0, -140.0), (-180.0, 250.0), (-150.0, -180.0), (190.0, 120.0), (-180.0, 280.0), (270.0, 60.0), (-130.0, 160.0), (110.0, -250.0), (190.0, -100.0), (-210.0, 150.0), (250.0, -180.0), (-160.0, 140.0), (210.0, -150.0), (-170.0, 180.0), (-200.0, -260.0), (180.0, 200.0), (-140.0, -250.0), (-220.0, 170.0), (130.0, -160.0), (200.0, -100.0), (220.0, -180.0), (-130.0, 190.0), (240.0, -120.0), (-260.0, 160.0), (190.0, -110.0), (150.0, 220.0), (170.0, -130.0), (-280.0, 80.0), (-220.0, 150.0), (200.0, -90.0), (160.0, -160.0), (210.0, 180.0), (-190.0, -140.0), (250.0, 100.0), (-180.0, 130.0), (170.0, -160.0), (-200.0, 240.0), (-140.0, -170.0), (-150.0, 180.0), (130.0, -260.0), (160.0, -100.0), (-120.0, 210.0), (140.0, -190.0), (230.0, -80.0), (-170.0, 200.0), (120.0, -150.0), (-140.0, 220.0), (180.0, -170.0), (-110.0, 190.0)]
