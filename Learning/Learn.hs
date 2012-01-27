import System (getArgs)
import qualified Data.Map as Map
import Debug.Trace
traceS a = trace $ show a

main = do
  cs <- readFile "merged.txt"
  print $ take 10 (lines cs)


