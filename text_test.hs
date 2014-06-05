import           FRP.Elerea.Simple  (Signal)
import           FRP.Elerea.Simple  (SignalGen)
import           FRP.Helm           (Element)
import           FRP.Helm           (square)
import           FRP.Helm           (red)
import           FRP.Helm           (filled)
import           FRP.Helm           (collage)
import           FRP.Helm           (defaultConfig)
import           FRP.Helm           (run)
import           FRP.Helm           (Time)
import           FRP.Helm           (toForm)
import           FRP.Helm           (centeredCollage)
import           FRP.Helm           ((~~))
import           FRP.Helm.Graphics  (move)
import qualified FRP.Helm.Text      as Text
import           FRP.Helm.Time      (fps)
import           FRP.Helm.Time      (delay)
import qualified FRP.Helm.Time      as Time
import           FRP.Helm.Utilities ((<~))
import qualified FRP.Helm.Window    as Window
import FRP.Elerea.Simple (transfer)

render :: Time -> Time -> (Int, Int) -> Element
render dt total_time (w, h) = collage w h [move ((fromIntegral w/2), total_time / 5) $ toForm $ Text.text $ Text.toText ("time " ++ show(dt) ++ " total " ++ show(total_time))]

input :: SignalGen (Signal Time)
input = delay (fps 60)

cumulativeTime :: SignalGen (Signal Time)
cumulativeTime = do
  frames <- input
  transfer 0 (\x y -> x + y) frames


main :: IO ()
main = run defaultConfig $ render <~ input ~~ cumulativeTime ~~ Window.dimensions
