import Prelude hiding (Either(..))
import qualified FRP.Elerea.Simple as El hiding (delay)
import FRP.Helm
import FRP.Helm.Time
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard

-- CONSTANTS (meaning a constant that might change)
windowWidth :: Double
windowWidth = 500

func :: Floating a => a -> a
func x = 100*(sin (x/40))
--func x = (- x * 0.5)

zeroedFunc :: Double -> Double
zeroedFunc = (snapToPositiveZero func) 0 windowWidth
-- MODEL SECTION

-- Gripper x y are the x and y coordinates of the gripper.
-- The bottom left corner of the window is (0,0) with positive being up and right
data Gripper = Gripper {gripX :: Double, gripY::Double, gripX'::Double, gripY'::Double} deriving Show

--Sensor values (left or right) should be true if the sensor is inside an obstacle
data Sensors = Sensors {left :: Bool, right :: Bool} deriving Show

--evaluats f in the domain [lowerBound, upperBonud] and makes sure that
-- the outupt function is >= 0 and its minimum value within the domain is 0
snapToPositiveZero :: (Enum t, Num a, Ord a) => (t -> a) -> t -> t -> t -> a
snapToPositiveZero f lowerBound upperBound = 
    \x -> f x - minValue where
    minValue = minimum [f x | x <- [lowerBound .. upperBound]]

-- UPDATE SECTION

physics :: Time -> Gripper -> Gripper
physics t (Gripper mX mY vX vY)
  = Gripper (mX + t * vX) (min 0 (mY - t * vY)) vX vY

--x1, y1 are the 

updateSensors :: Ord a => t -> a -> t -> a -> (t -> a) -> Sensors
updateSensors xLeft yLeft xRight yRight f =
    Sensors {left = f xLeft >= yLeft, right = f xRight >= yRight}

step :: (Time, (Int, Int)) -> Gripper -> Gripper
step (t, _) = physics t

-- VIEW SECTION

render :: Gripper -> (Int, Int) -> Element
render (Gripper mX1 mY1 _ _ ) (w, h) =
  collage w h [
               move (half w, half h) $ filled white $ rect (fromIntegral w) (fromIntegral h), 
               move (0, fromIntegral h) $ filled green obstacle,
               move (mX1, mY1 + fromIntegral h) mario_shape
              ] 
  where
    half = (/ 2). fromIntegral
    mario_height = 50 :: Double
    mario_width = 28 :: Double
    mario_shape = filled red $ triangle mario_width mario_height
    triangle width height = polygon [(0,0), (width, 0), (width/2, -height)]
    
    
obstacle :: Shape
obstacle = funcToShape (floor . zeroedFunc . fromIntegral) (floor windowWidth)

--render (Gripper mX1 mY1 _ _ ) (w, h) =
--  centeredCollage w h [filled green $ funcToShape (*2) w]

--TODO shift the function up so that the function does not need to have positive values
--Takes a simple function from Integrals to Integrals and produces a shape
--min and max are the range over which the function will be evaluated from 0 to max
funcToShape :: (Integral a) => (a -> a) -> a -> Shape
funcToShape f m = polygon $ (0,0):[(fromIntegral x, -(fromIntegral $ f x)) | x <-[0..m]] ++ [(fromIntegral m, 0)]

-- SIGNALS SECTION

input :: El.SignalGen (El.Signal (Time, (Int, Int)))
input = lift2 (,) delta' Keyboard.arrows
  where
    delta' = lift (/15) $ delay $ fps 60

-- MAIN

main :: IO ()
main =
    run config $ render <~ stepper ~~ Window.dimensions

  where

    window_height = 500 :: Double
    mario = Gripper (windowWidth / 2) (-200) 0 (-10)
    stepper = foldp step mario input
    config = defaultConfig{
               windowDimensions = (floor windowWidth, floor window_height),
               windowIsResizable = False}
