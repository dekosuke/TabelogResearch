import Network.Shpider
import System.Random (getStdRandom, randomR)
import Control.Concurrent (threadDelay)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.List (sort)
import Data.List.Utils (join, contains, replace) -- MissingH library
import System (getArgs)
import qualified Data.Map as Map
import Debug.Trace
import System.IO (stdout, hFlush)
import Text.Regex
import Directory (getDirectoryContents)
import Codec.Binary.UTF8.String (encodeString, decodeString)
import Text.HTML.TagSoup --HTMLパーズのライブラリ
traceS a = trace $ show a

--レビューからユーザ名、昼のスコア、夜のスコアを返す
extractScore :: [Tag String] -> (String, String, String)
extractScore wrap = 
  let user = (filter isTagOpen (head $ sections (~== "<p class=\"reviewer-name\">") wrap)) !! 1 in
  case user of
    (TagOpen _ attrs) -> (retrieveUser$snd$head$attrs, dayScore wrap, nightScore wrap)

--http://u.tabelog.com/000132333/ -> 000132333
retrieveUser ss = 
  case matchRegex (mkRegex "http://u.tabelog.com/(.+?)/") ss of
    Nothing -> error "hoge"
    Just as -> head as

unTagText (TagText s) = s

dayScore = tabeScore "<li class=\"ratings-lunch clearfix\">"
nightScore = tabeScore "<li class=\"ratings-dinner clearfix\">"
tabeScore exp wrap = 
  case partitions (~== exp) wrap of
    [] -> "-"
    (x:xs) ->
      case partitions (~== "<strong>") x of
        [] -> "-"
        a -> unTagText (a !! 0 !! 1)

extractPage :: String -> IO ()
extractPage filename = do
  putStrLn filename
  cst <- readFile filename
  let cs = encodeString cst
  let tags = parseTags cs
  let ss = sections (~== "<span property=\"v:average\">") tags
  let wraps = partitions (~== "<div class=\"review-wrap\">") tags
  --let wrap_days = map (\w -> head $  partitions (~== "<li class=\"ratings-lunch clearfix\">") w) wraps
  --let wrap_nights = map (\w -> partitions (~== "<li class=\"ratings-dinner clearfix\">") w) wraps
  --let wrap_days_scores = map (\w -> partitions (~== "<strong>") w !! 0 !! 1) wrap_days
  --let wrap_night_scores = map extractScore wrap_nights
--print $ (head ss) !! 1
--print $ fmap (\w->(filter isTagOpen (head $ sections (~== "<p class=\"reviewer-name\">") w)) !! 1) wraps 
  mapM_ (putStrLn.joinTuple3) $ map extractScore wraps
  --print wrap_days_scores
  --print wrap_nights

joinTuple3 (a,b,c) = a ++ "," ++ b ++ "," ++ c

--url=http://r.tabelog.com/tokyo/A1322/A132205/13085868/dtlrvwlst/
--"r.tabelog/tA1309_A130905_13024910_dtlrvwlst_COND-0_smp2_?PG=2&lc=0"
 
findReviewPages :: String -> IO [String]
findReviewPages fileDir = do
  cs <- getDirectoryContents fileDir
  return ["../data/r.tabelog/"++c|c<-cs,
    matchRegex (mkRegex "t[A-Z][0-9]+_[A-Z][0-9]+_[0-9]+_dtlrvwlst_") c /= Nothing]

 
--main = extractPage "../data/r.tabelog/tA1309_A130905_13024910_dtlrvwlst_"
main = do
  fs <- findReviewPages "../data/r.tabelog/"
  --print $ head fs
  mapM_ extractPage fs


