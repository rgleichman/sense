import           FRP.Helm           (Element)
import           FRP.Helm           (square)
import           FRP.Helm           (red)
import           FRP.Helm           (filled)
import           FRP.Helm           (collage)
import           FRP.Helm           (defaultConfig)
import           FRP.Helm           (run)
import           FRP.Helm.Graphics  (move)
import           FRP.Helm.Utilities ((<~))
import qualified FRP.Helm.Window    as Window

render :: (Int, Int) -> Element
render (w, h) = collage w h [move (100, 200) $ filled red $ square 64]

main :: IO ()
main = run defaultConfig $ render <~ Window.dimensions
