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
traceS a = trace $ show a

convertUrlToFilename :: String -> String -> String
convertUrlToFilename baseUrl url = 
  "r.tabelog/t" ++ replace "/" "_" (drop l url)
  where l = length baseUrl

--htmlページの取得と保存
getLinks :: String -> String -> IO [String]
getLinks baseUrl url =
  do
    page <- runShpider $ do
      (_, page) <- download url
      return page
    let (html, linkUrls) = (source page, links page)
    writeFile filename html
    return $ fmap linkAddress linkUrls
  where filename = convertUrlToFilename baseUrl url

filterLinks :: String -> [String] -> [String]
filterLinks baseUrl urls =
  let urlSubs = [drop l url | url<-urls, take l url == baseUrl] in --baseUrlを削った
  [baseUrl++urlSub | urlSub<-urlSubs,
    not $ contains "/rst/" urlSub,
    not $ contains "/rvw/" urlSub,
    not $ contains "/lst/" urlSub,
    not $ contains "/favoritelst/" urlSub,  
    not $ contains "/dtlmenu/" urlSub,
    not $ contains "/dtlmap/" urlSub,
    not $ contains "/dtlprecoupon/" urlSub,  
    not $ contains "/peripheral_map/" urlSub,  
    not $ contains "#" urlSub,  
    not $ contains "/kdwr" urlSub,  
    not $ contains "0/0/" urlSub,  
    not $ contains "COND" urlSub,  
    not $ contains "new_open" urlSub,  
    not $ contains "photo" urlSub,  
    not $ contains "jpg" urlSub,  
    not $ contains "png" urlSub,  
    not $ contains "gif" urlSub,  
    not $ contains "javascript" urlSub,  
    not $ contains "/dtlphotolst/" urlSub,
    not $ contains "/dtlbook/" urlSub,
    not $ contains "/dtlmobile/" urlSub,
    not $ contains "/dtlratings/" urlSub,
    not $ contains "/dtlrvwlst/" urlSub,
    not $ contains "shinnennkai" urlSub,
    not $ contains "/dtlkodawari/" urlSub,
    not $ contains "coupon" urlSub,
    matchRegex (mkRegex "dtlrvwlst/[0-9]") urlSub == Nothing
  ] --ここがメインフィルタ
  where l = length baseUrl

crawl :: String -> Map.Map String Int -> Map.Map String Int -> IO ()
crawl baseUrl pathsToVisit pathsVisited = 
  if Map.size pathsToVisit == 0 then return () --no path left
  else do
    let currentUrl = fst $ Map.findMin pathsToVisit
    let pathsVisitedUpdate = Map.insert currentUrl 1 pathsVisited
    print $ "now : " ++ currentUrl
    print $ "left : " ++ show (Map.size pathsToVisit)
    hFlush stdout
    threadDelay 1000 --sleep 1sec
    links <- getLinks baseUrl currentUrl
    let flinks = filterLinks baseUrl links
    let linksMap = Map.difference (Map.fromList $ zip flinks (cycle [1])) pathsVisitedUpdate
    crawl baseUrl (Map.union (Map.deleteMin pathsToVisit) linksMap) pathsVisitedUpdate

main = do
  crawl "http://r.tabelog.com/tokyo/" 
    (Map.fromList [("http://r.tabelog.com/tokyo/",1)]) Map.empty
