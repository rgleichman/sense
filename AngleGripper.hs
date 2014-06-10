module AngleGripper where
import Prelude hiding (Either(..))
import qualified FRP.Elerea.Simple as El hiding (delay)
import FRP.Helm
import FRP.Helm.Time
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard
import qualified System.Random as Rand
-- CONSTANT CONSTANTS (constants that won't change)
tau :: Double
tau = pi

-- CONSTANTS (meaning a constant that might change)
windowWidth :: (Num a) => a
windowWidth = 500

windowHeight :: (Num a) => a
windowHeight = 500 

func :: Double -> Double
func x = 100 * sin (tau * x/40)

gripperWidth :: (Num a) => a
gripperWidth = 28

zeroedFunc :: Double -> Double
zeroedFunc = snapToPositiveZero func 0 windowWidth

-- MODEL SECTION

-- Gripper x y are the x and y coordinates of the gripper.
-- The bottom left corner of the window is (0,0) with positive being up and senRight
data Gripper = Gripper {gripPosVel :: PosAndVel, gripSensors::Sensors, gripRands::[Double]} deriving Show

type LinearVelocity = Double
type AngularVelocity = Double
type Position = Double
type Angle = Double
data PosAndVel = PosAndVel {poseX :: Position, poseY::Position, velX::LinearVelocity, velY::LinearVelocity, poseTheta::Angle, velTheta::AngularVelocity}
                 deriving Show

--Sensor values (left or senRight) should be true if the sensor is inside an obstacle
data Sensors = Sensors {senLeft :: Bool, senRight :: Bool} deriving Show

sensorPose :: PosAndVel -> (Position, Position, Position, Position)
sensorPose PosAndVel{poseX=x, poseY=y, poseTheta=theta} =
  (leftX, leftY, rightX, rightY) where
    xVec = gripperWidth/2 * cos theta
    yVec = gripperWidth/2 * sin theta
    rightX = x + xVec
    rightY = y + yVec
    leftX = x - xVec
    leftY = y - yVec

--evaluats f in the domain [lowerBound, upperBonud] and makes sure that
-- the outupt function is >= 0 and its minimum value within the domain is 0
snapToPositiveZero :: (Enum t, Num a, Ord a) => (t -> a) -> t -> t -> t -> a
snapToPositiveZero f lowerBound upperBound = 
    \x -> f x - minValue where
    minValue = minimum [f x | x <- [lowerBound .. upperBound]]

-- UPDATE SECTION

-- clamps the value between bottom and top
-- if bottom > top then return bottom
clamp :: Ord a => a -> a -> a -> a
clamp bottom top value = max bottom $ min top value

--The velX and velY are approximately pixels/second
--vTheta is approximately in radians/second
physics :: Time -> (Int, Int) -> Gripper -> Gripper
physics t (width, height) oldGripper@Gripper{gripPosVel=oldPose@PosAndVel{poseX=x, poseY=y, velX = x', velY = y',poseTheta=theta, velTheta= vTheta}}
  = oldGripper{gripPosVel = oldPose{poseX = clamp 0 (fromIntegral width) (x + (deltaSeconds * x'))
                                   ,poseY = clamp 0 (fromIntegral height) (y + (deltaSeconds * y'))
                                   ,poseTheta = theta + (deltaSeconds*vTheta)
                                   }
              }    
    where deltaSeconds = inSeconds t
       
updateSensors :: Ord a => t -> a -> t -> a -> (t -> a) -> Sensors
updateSensors xSenLeft ySenLeft xSenRight ySenRight f =
    Sensors {senLeft = f xSenLeft >= ySenLeft, senRight = f xSenRight >= ySenRight}

-- The gripper position coordinates are in the lower left corner of the gripper
-- with +x being to the right and +y 90 degrees counterclockwise from +x
updateGripperSensors :: Gripper -> Gripper
updateGripperSensors gripper@Gripper{gripPosVel = pose} =
  let (leftX, leftY, rightX, rightY) = sensorPose pose
      newSensors = updateSensors leftX leftY rightX rightY zeroedFunc
  in 
   gripper{gripSensors = newSensors}

--Controls the gripper to align to obstacles

gripperController :: (LinearVelocity, AngularVelocity) -> Gripper -> Gripper
gripperController (linearSpeed, angularSpeed) oldGripper@Gripper{gripPosVel=oldPose@PosAndVel{poseTheta=theta},
                                                                 gripSensors = Sensors{senLeft = left, senRight = right}
                                                                }
  = oldGripper{gripPosVel = oldPose{velX = newX'
                                  ,velY = newY'
                                  ,velTheta = newTheta'
                                  }
              }
   where
     centerSpeed = angularSpeed*gripperWidth/2
     relativeToAbsCoords relX relY = ((relX*cos theta) - (relY*sin theta),
                                            (relY*cos theta) + (relX*sin theta))
     ((newX', newY'), newTheta') = case (left, right) of
       (False, False) -> (relativeToAbsCoords 0 (-linearSpeed), 0)
       (True, False) -> (relativeToAbsCoords 0 (-centerSpeed), -angularSpeed)
       (False, True) -> (relativeToAbsCoords 0 (-centerSpeed), angularSpeed)
       (True, True) -> (relativeToAbsCoords 0 linearSpeed, 0)


addVelocityNoise :: RealFrac a => a -> (LinearVelocity, AngularVelocity) -> Gripper -> Gripper
addVelocityNoise rand (linearNoise, angularNoise) oldGripper@Gripper{gripPosVel=oldPose@PosAndVel{velX=vX, velY = vY, velTheta=vT}}
  =
   oldGripper{gripPosVel = oldPose{velX = vX + linearNoise*rand1
                                  ,velY = vY + linearNoise*rand2
                                  ,velTheta=vT+ angularNoise*rand3
                                  }
             ,gripRands = rands
             }
   where
     rands@[rand1, rand2, rand3] = take 3 $ Rand.randomRs (-1, 1) (Rand.mkStdGen $ floor $ 1000*rand)::[Double]

randomRanges :: (Rand.RandomGen b, Rand.Random a) => [(a, a)] -> b -> [a]
randomRanges ranges gen = fst $ foldr foldFunc ([], gen) ranges
                          where
                            foldFunc (low, high) (results, fGen) = (randNum:results, newGen)
                              where
                                  (randNum, newGen) = Rand.randomR (low, high) fGen
                                  
step :: (Time, t1, (Int, Int), Double) -> Gripper -> Gripper
step (t, _, windowDim, rand)  gripper = physics t windowDim $
                                  addVelocityNoise rand (linearNoise, angularNoise)$
                                  gripperController (linearSpeed, angularSpeed) $
                                  updateGripperSensors gripper
  where
    linearSpeed = 100 --pixes/second
    angularSpeed = tau/2 --radians/second
    linearNoise = linearSpeed --Uniform [-linearNoise, linearNoise]
    angularNoise = angularSpeed --Uniform [-angularSpeed, angularSpeed]

-- view SECTION

render :: Gripper -> (Int, Int) -> Element
render Gripper{gripPosVel=PosAndVel{poseX=x, poseY=y, poseTheta=theta}
              ,gripSensors=Sensors{senLeft=left, senRight=right}}
  (w, h) =
  collage w h [
    -- Draw the background
    move (half w, half h) $ filled white $ rect (fromIntegral w) (fromIntegral h),
    --Draw the obstacle
    move (0, fromIntegral h) $ filled green obstacle,
    --Draw the gripper
    move (x, (-y) + fromIntegral h) $ rotateCC theta $ group [
      gripper_shape,
      --Dray the left sensor
      drawSensor (-gripperWidth/2) 0 left,
      --Draw the right sensor
      drawSensor (gripperWidth/2) 0 right
      ]
    ] 
  where
    half = (/ 2). fromIntegral
    gripper_height = 50 :: Double
    gripper_shape = filled red $ triangle gripperWidth gripper_height
    triangle width height = polygon [(-width/2,0), (width/2, 0), (0, -height)]
    drawSensor xx yy blocked = move (xx, -yy) $ outlined  (solid (if blocked then yellow else black)){lineWidth=gripperWidth/8} $ circle (gripperWidth / 4)
    
obstacle :: Shape
obstacle = funcToShape zeroedFunc windowWidth

--Takes a simple function from Integrals to Integrals and produces a shape
--min and max are the range over which the function will be evaluated from 0 to max
funcToShape :: (Double -> Double) -> Double -> Shape
funcToShape f m = polygon $ (0,0):[(x, -(f x)) | x <-[0..m]] ++ [(m, 0)]

-- A positive angle will rotate counter clockwise
rotateCC :: Double -> Form -> Form
rotateCC angle = rotate (-angle)

-- SIGNALS SECTION

input :: El.SignalGen (El.Signal (Time, (Int, Int), (Int, Int), Double))
input = lift4 (\x y z w-> (x,y,z,w)) delta' Keyboard.arrows Window.dimensions (randomR (-1, 1))
  where
    delta' = delay $ fps 60

-- MAIN

main :: IO ()
main =
    run config $ render <~ stepper ~~ Window.dimensions
  where
    initialGripper =
      let
        defaultSensors = Sensors{senLeft=False, senRight=False}
        defaultPosAndVel = PosAndVel{poseX = 0, poseY = 0, velX = 0, velY = 0, poseTheta = 0, velTheta = 0}
      in
       Gripper{gripPosVel = defaultPosAndVel{
                  poseX = 120
                  ,poseY = 400
                  --,velX = 0.5
                  --,velY = -0.1
                  ,velY = -100
                  --,poseTheta = tau/12
                  --,velTheta = tau/100
                  ,velTheta = 0                              
                  },
               gripSensors = defaultSensors}
    stepper = foldp step initialGripper input
    config = defaultConfig{
               windowDimensions = (windowWidth, windowHeight),
               windowIsResizable = False}
