import Prelude hiding (Either(..))
import qualified FRP.Elerea.Simple as El hiding (delay)
import FRP.Helm
import FRP.Helm.Time
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard

data Gripper = Gripper Double Double Double Double deriving Show

physics :: Time -> Gripper -> Gripper
physics t (Gripper mX mY vX vY)
  = Gripper (mX + t * vX) (min 0 (mY - t * vY)) vX vY

step :: (Time, (Int, Int)) -> Gripper -> Gripper
step (t, _) = physics t

render :: Gripper -> (Int, Int) -> Element
render (Gripper mX1 mY1 _ _ ) (w, h) =
  collage w h [
               move (half w, half h) $ filled white $ rect (fromIntegral w) (fromIntegral h), 
               move (half w, fromIntegral h - half bh) $ filled black $ rect (fromIntegral w) (fromIntegral bh),
               move (0, fromIntegral $ h - bh) $ filled green $ obstacle,
               move (mX1, mY1 + fromIntegral h - fromIntegral bh) $ mario_shape
              ] 
  where
    half = (/ 2). fromIntegral
    bh = 50
    mario_height = 50 :: Double
    mario_width = 28 :: Double
    mario_shape = filled red $ triangle mario_width mario_height
    triangle width height = polygon [(0,0), (width, 0), (width/2, -height)]
    
func :: Floating a => a -> a
func x = 100*(1 + sin (x/40))
    
window_width :: Double
window_width = 500
obstacle :: Shape
obstacle = func_to_shape (floor . func . fromIntegral) (floor window_width)

--render (Gripper mX1 mY1 _ _ ) (w, h) =
--  centeredCollage w h [filled green $ func_to_shape (*2) w]

--Takes a simple function from Integrals to Integrals and produces a shape
--min and max are the range over which the function will be evaluated from 0 to max
func_to_shape :: (Integral a) => (a -> a) -> a -> Shape
func_to_shape f m = polygon $ (0,0):[(fromIntegral x, -(fromIntegral $ f x)) | x <-[0..m]] ++ [(fromIntegral m, 0)]

input :: El.SignalGen (El.Signal (Time, (Int, Int)))
input = lift2 (,) delta' Keyboard.arrows
  where
    delta' = lift (/15) $ delay $ fps 60

main :: IO ()
main = do
    run config $ render <~ stepper ~~ Window.dimensions

  where

    window_height = 500 :: Double
    mario = Gripper (window_width / 2) (-200) 0 (-10)
    stepper = foldp step mario input
    config = defaultConfig{
               windowDimensions = (floor(window_width), floor(window_height)),
               windowIsResizable = False}
