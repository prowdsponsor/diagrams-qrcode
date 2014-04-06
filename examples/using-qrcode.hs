import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)   -- blaze-svg
import qualified Codec.Binary.QRCode as QR        -- qrcode
import qualified Data.ByteString.Lazy.Char8 as L8 -- bytestring
import qualified Diagrams.Backend.SVG as D        -- diagrams-svg
import qualified Diagrams.Prelude as D            -- diagrams-lib
import qualified Diagrams.QRCode as QR            -- diagrams-qrcode

main :: IO ()
main = do
  -- Read input from stdin
  input <- getContents

  -- Calculate QR code
  let Just ver    = QR.version 3 {- arbitrary, but needed by qrcode lib -}
      Just qrcode = QR.encode ver QR.M QR.Alphanumeric input

  -- Draw QR code
  let dia = D.scale 6 $ QR.stroke $ QR.pathArray $ fmap not $ QR.toArray qrcode

  -- Render diagram
  L8.putStrLn $ renderSvg $ D.renderDia D.SVG (D.SVGOptions D.Absolute Nothing) dia
