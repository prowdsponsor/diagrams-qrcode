import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)   -- blaze-svg
import qualified Data.ByteString.Lazy.Char8 as L8 -- bytestring
import qualified Data.QRCode as QR                -- haskell-qrencode
import qualified Diagrams.Backend.SVG as D        -- diagrams-svg
import qualified Diagrams.Prelude as D            -- diagrams-lib
import qualified Diagrams.QRCode as QR            -- diagrams-qrcode

main :: IO ()
main = do
  -- Read input from stdin
  input <- getContents

  -- Calculate QR code
  qrcode <- QR.encodeString input Nothing QR.QR_ECLEVEL_M QR.QR_MODE_EIGHT True

  -- Draw QR code
  let dia = D.scale 6 $ QR.stroke $ QR.pathMatrix $ QR.toMatrix qrcode

  -- Render diagram
  L8.putStrLn $ renderSvg $ D.renderDia D.SVG (D.SVGOptions D.Absolute) dia
