import Network.Shpider
import System.Random (getStdRandom, randomR)
import Control.Concurrent (threadDelay)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.List (sort)
import Data.List.Utils (contains, replace) -- MissingH library
import System (getArgs)
import qualified Data.Map as Map
import Debug.Trace
import System.IO (stdout, hFlush)
import Text.Regex
import Directory (getDirectoryContents)
import Codec.Binary.UTF8.String (encodeString, decodeString)
import Text.Html.TagSoup --HTMLパーズのライブラリ
traceS a = trace $ show a

extractPage :: String -> IO ()
extractPage filename = do
  cst <- readFile filename
  let cs = encodeString cst

--url=http://r.tabelog.com/tokyo/A1322/A132205/13085868/dtlrvwlst/
--"r.tabelog/tA1309_A130905_13024910_dtlrvwlst_COND-0_smp2_?PG=2&lc=0"
 
findReviewPages :: String -> IO [String]
findReviewPages fileDir = do
  cs <- getDirectoryContents fileDir
  return [c|c<-cs,
    matchRegex (mkRegex "t[A-Z][0-9]+_[A-Z][0-9]+_[0-9]+_dtlrvwlst_(?:COND-0_.+PG.+_)?$") c /= Nothing]

 
main = extractPage "../data/r.tabelog/tA1309_A130905_13024910_dtlrvwlst_"


