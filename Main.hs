module Main where

import           Control.Applicative           (liftA2)
import           Data.List                     (scanl')
import           Data.Maybe                    (mapMaybe)
import           Graphics.Rendering.Chart.Easy (layout_title, line, plot, (.=))
import           Graphics.Rendering.Chart.Gtk  (toWindow)
import           System.Environment            (getArgs)
import           System.Process                (readProcess)
import           Text.Read                     (readMaybe)

numStat :: [String] -> Maybe (Int, Int)
numStat (a:b:_) =
  liftA2 (,) (readMaybe a) (readMaybe b)
numStat _ =
  Nothing

main :: IO ()
main = do
  args <- getArgs
  result <- readProcess "git" (["log", "--format=", "--numstat", "--reverse"] ++ args) ""
  let numStats = mapMaybe (numStat . words) (lines result)
      netStats = scanl' (flip ((+) . uncurry (-))) 0 numStats
  toWindow 500 200 $ do
    layout_title .= "Repository Impact"
    plot (line "lines" [zip [(0::Int)..] netStats])
