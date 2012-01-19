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
traceS a = trace $ show a

--クローラ、ユーザレビュー取得向け
convertUrlToFilename :: String -> String -> String
convertUrlToFilename baseUrl url = 
  "r.tabelog/t" ++ replace "/" "_" (drop l url)
  where l = length baseUrl

--r.tabelog/
--tA1301_A130101_13002370_
--クロール対象のページを店のリスト(正確にはファイル名のリスト)を取得する
findShopPages :: String -> IO [String]
findShopPages fileDir = do
  cs = getDirectoryContents fileDir
  let csFilt = [c|c<-cs,
    matchRegex (mkRegex "t[A-Z][0-9]+_[A-Z][0-9]+_[0-9]+") c /= Nothing]
  return csFilt

--ページを探索する
parsePage :: String -> IO ()
parsePage filename = do
  cs <- getContents filename --ファイル開いて
  
