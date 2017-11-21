module Main where

import qualified Data.Vector.Generic       as V
import qualified Graphics.Image            as G
import qualified Graphics.Image.ColorSpace as C
import qualified Graphics.Image.Interface  as I
import           Numeric                   (showHex)
import           System.Environment        (getArgs)

toPixelArr :: String -> IO String
toPixelArr image = do
  img <- G.readImageRGB G.VU image
  let (w, h) = G.dims img
  let pixels = V.toList $ I.toVector img
  return $ concat [unlines $ map (`showHex` "") [w, h], "18\n", unwords $ map rgbToHexF pixels]

rgbToHexF :: (Show a, RealFrac a) => I.Pixel C.RGB a -> String
rgbToHexF (C.PixelRGB r g b) = concatMap ezHex actual
  where ezHex x = showHex x ""
        actual = map (floor . (* 255)) [r, g, b]

exec :: [String] -> IO ()
exec args
  | null args || length args > 1 = putStrLn "usage: c_org <image>"
  | otherwise = toPixelArr (head args) >>= putStrLn

main :: IO ()
main = do
  args <- getArgs
  exec args
