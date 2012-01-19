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
traceS a = trace $ show a

--クローラ、ユーザレビュー取得向け
convertUrlToFilename :: String -> String -> String
convertUrlToFilename baseUrl url = 
  "r.tabelog/t" ++ replace "/" "_" (drop l url)
  where l = length baseUrl

convertFilenameToUrl :: String -> String 
convertFilenameToUrl filename = 
  "http://r.tabelog.com/tokyo/" ++ replace "_" "/" (drop 1 filename) ++ "dtlrvwlst/"

--r.tabelog/
--tA1301_A130101_13002370_
--クロール対象のページを店のリスト(正確にはファイル名のリスト)を取得する
findShopPages :: String -> IO [String]
findShopPages fileDir = do
  cs <- getDirectoryContents fileDir
  return [c|c<-cs,
    matchRegex (mkRegex "t[A-Z][0-9]+_[A-Z][0-9]+_[0-9]+") c /= Nothing]

--ページを探索し、レビュワーのリストと（あれば）次ページを返す
parsePageContents :: String -> IO (Maybe String)
parsePageContents cs = do
  let nextRegex = mkRegex (encodeString "<a href=\"([^\"]+)\" rel=\"next\">次の20件")
  let next = matchRegex nextRegex cs
  print $ "next=" ++ show next
  case next of 
    Just m -> return $ Just (head m)
    _ -> return Nothing

crawlPage :: String -> IO ()
crawlPage filename = do
  print filename 
  crawlPage' $convertFilenameToUrl filename

crawlPage' :: String -> IO ()
crawlPage' url = do 
  print url
  page <- runShpider $ do
    (_, page) <- download url
    return page
  threadDelay 100000 --sleep 1sec
  let cs = source page
  let filename = convertUrlToFilename "http://r.tabelog.com/tokyo/" url
  print $ filename
  writeFile filename $ decodeString cs
  next <- parsePageContents cs
  case next of 
    Just nextUrl -> crawlPage' $ "http://r.tabelog.com" ++ nextUrl
    Nothing -> return ()

crawl :: IO ()
crawl = do
  shopPages <- findShopPages "r.tabelog/"
  print $ length shopPages
  let pages = take 100 shopPages
  mapM_ crawlPage pages
  
main = do
  let page = "tA1315_A131503_13058503/"
  crawlPage page
