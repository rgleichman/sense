module AngleGripper where
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
func x = 100 * sin (x/40)
--func x = (- x * 0.5)

zeroedFunc :: Double -> Double
zeroedFunc = snapToPositiveZero func 0 windowWidth
-- MODEL SECTION

-- Gripper x y are the x and y coordinates of the gripper.
-- The bottom left corner of the window is (0,0) with positive being up and senRight
data Gripper = Gripper {gripPosVel :: PosAndVel, gripSensors::Sensors} deriving Show

type LinearVelocity = Double
type AngularVelocity = Double
type Position = Double
type Angle = Double
data PosAndVel = PosAndVel {poseX :: Position, poseY::Position, velX::LinearVelocity, velY::LinearVelocity, poseTheta::Angle, velTheta::AngularVelocity}
                 deriving Show
                          
--Sensor values (left or senRight) should be true if the sensor is inside an obstacle
data Sensors = Sensors {senLeft :: Bool, senRight :: Bool} deriving Show

--evaluats f in the domain [lowerBound, upperBonud] and makes sure that
-- the outupt function is >= 0 and its minimum value within the domain is 0
snapToPositiveZero :: (Enum t, Num a, Ord a) => (t -> a) -> t -> t -> t -> a
snapToPositiveZero f lowerBound upperBound = 
    \x -> f x - minValue where
    minValue = minimum [f x | x <- [lowerBound .. upperBound]]

-- UPDATE SECTION

--TODO: clip x and y such that the gripper does not go off screen
physics :: Time -> Gripper -> Gripper
physics t oldGripper@Gripper{gripPosVel=pose}
  = oldGripper{gripPosVel = newPose}
    where      
      newPose = case pose of
       PosAndVel{poseX=x, poseY=y, velX = x', velY = y'} -> pose{poseX = x + t * x', poseY = min 0 (y - t * y')}
       
--x1, y1 are the 

updateSensors :: Ord a => t -> a -> t -> a -> (t -> a) -> Sensors
updateSensors xSenLeft ySenLeft xSenRight ySenRight f =
    Sensors {senLeft = f xSenLeft >= ySenLeft, senRight = f xSenRight >= ySenRight}

updateGripperSensors :: Gripper -> Gripper
updateGripperSensors = id

step :: (Time, (Int, Int)) -> Gripper -> Gripper
step (t, _)  gripper= updateGripperSensors $ physics t gripper

-- VIEW SECTION

render :: Gripper -> (Int, Int) -> Element
render Gripper{gripPosVel=PosAndVel{poseX=x, poseY=y}}  (w, h) =
  collage w h [
               move (half w, half h) $ filled white $ rect (fromIntegral w) (fromIntegral h), 
               move (0, fromIntegral h) $ filled green obstacle,
               move (x, y + fromIntegral h) gripper_shape
              ] 
  where
    half = (/ 2). fromIntegral
    gripper_height = 50 :: Double
    gripper_width = 28 :: Double
    gripper_shape = filled red $ triangle gripper_width gripper_height
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
    initialGripper =
      let
        defaultSensors = Sensors{senLeft=False, senRight=False}
        defaultPosAndVel = PosAndVel{poseX = 0, poseY = 0, velX = 0, velY = 0, poseTheta = 0, velTheta = 0}
      in
       Gripper{gripPosVel = defaultPosAndVel{poseX = windowWidth / 2, poseY = -200, velX = 0, velY = -10},
               gripSensors = defaultSensors}
    stepper = foldp step initialGripper input
    config = defaultConfig{
               windowDimensions = (floor windowWidth, floor window_height),
               windowIsResizable = False}
