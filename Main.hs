module Main where

import           Control.Applicative           (liftA2)
import           Data.Foldable                 (for_)
import           Data.List                     (scanl')
import           Data.List.Split               (splitOn)
import           Data.Maybe                    (mapMaybe)
import           Graphics.Rendering.Chart.Easy
import           Graphics.Rendering.Chart.Gtk  (toWindow)
import           System.Environment            (getArgs)
import           System.Process                (readProcess)
import           Text.Read                     (readMaybe)

numStat :: [String] -> Maybe (Int, Int)
numStat (a:b:_) =
  liftA2 (,) (readMaybe a) (readMaybe b)
numStat _ =
  Nothing

runGit :: [String] -> IO String
runGit args =
  readProcess "git" (["log", "--format=", "--numstat", "--reverse"] ++ args) ""

parseStats :: String -> [Int]
parseStats = do
  scanl' (flip ((+) . uncurry (-))) 0  . mapMaybe (numStat . words) . lines

main :: IO ()
main = do
  args <- splitOn ["--"] <$> getArgs
  stats <- traverse (\as -> (,) (unwords as) . parseStats <$> runGit as) args
  toWindow 500 200 $ do
    layout_title .= "Repository Impact"
    layout_x_axis . laxis_title .= "File edits"
    layout_y_axis . laxis_title .= "Lines"
    for_ stats $ \(l, s) ->
      plot (line l [zip [(0::Int)..] s])
