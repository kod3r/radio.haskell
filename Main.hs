{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP (getRequest, postRequestWithBody, getResponseBody, simpleHTTP)

import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero)

import Data.Aeson (FromJSON, (.:), (.:?), Value (Object), decode, parseJSON)
import Data.Aeson.Types as AT (Value (String,Number,Array,Bool))
import Data.Text as T (unpack)
import Data.Vector ((!))
import qualified Data.ByteString.Lazy.Char8 as B (ByteString, pack, unpack, drop, init)

import Text.ParserCombinators.Parsec
-- Main API Data and Stuff--

data RadioThread = RadioThread String deriving (Show, Eq)
data RadioInfo = RadioInfo {
  _online :: Int,
  np :: String,
  _list :: String,
  kbps :: Int,
  start :: Int,
  end :: Int,
  cur :: Int,
  dj :: String,
  djimg :: String,
  djtext :: String,
  djcolor :: String,
  thread :: RadioThread,
  lp :: [(String,String,Int)],
  queue :: Maybe [(String, String, String)]
  } deriving (Show, Eq)

online :: RadioInfo -> Bool
online = (==) 1 . _online

list :: RadioInfo -> Int
list = read . _list

instance FromJSON RadioThread where
  parseJSON (AT.String str) = do
    return $ RadioThread (T.unpack str)
  parseJSON (AT.Number num) = do
    return $ RadioThread ""
  parseJSON _ = mzero

instance FromJSON RadioInfo where
  parseJSON (Object o) = RadioInfo <$>
                         o .: "online" <*>
                         o .: "np" <*>
                         o .: "list" <*>
                         o .: "kbps" <*>
                         o .: "start" <*>
                         o .: "end" <*>
                         o .: "cur" <*>
                         o .: "dj" <*>
                         o .: "djimg" <*>
                         o .: "djtext" <*>
                         o .: "djcolor" <*>
                         o .: "thread" <*>
                         o .: "lp" <*>
                         o .:? "queue"
  parseJSON _ = mzero

-- Searching API Data and Stuff --

data SearchEntry = SearchEntry {
  artist :: String,
  title :: String,
  lastPlayed :: Integer,
  lastRequested :: Int,
  id :: String,
  requestable :: Bool
  } deriving (Show, Eq)

data SearchPage = SearchPage {
  status :: Bool,
  cooldown :: String,
  result :: [SearchEntry],
  pages :: Int,
  _page :: String
  } deriving (Show, Eq)

page :: SearchPage -> Int
page = read . _page

extractBool (AT.Bool a) = a
extractBool _ = False
extractInt (AT.Number a) = read $ show a
extractInt _ = 0
extractString (AT.String a) = T.unpack a
extractString _ = ""

instance FromJSON SearchEntry where
  parseJSON (Array a) = do
    return $ SearchEntry
      (extractString (a ! 0))
      (extractString (a ! 1))
      (extractInt (a ! 2))
      (extractInt (a ! 3))
      (extractString (a ! 4))
      (extractBool (a ! 5))
  parseJSON _ = mzero

instance FromJSON SearchPage where
  parseJSON (Object o) = SearchPage <$>
                         o .: "status" <*>
                         o .: "cooldown" <*>
                         o .: "result" <*>
                         o .: "pages" <*>
                         o .: "page"
  parseJSON _ = mzero

-- API URLS --
mainAPI = "http://r-a-d.io/api.php"
queueAPI = "http://r-a-d.io/queue/api.php"
searchAPI = "http://r-a-d.io/search/api.php?query="
lPlayAPI = "http://r-a-d.io/lastplayed/api.php"
requestAPI = "http://r-a-d.io/requests/index.py"
autoCompleteAPI = "http://r-a-d.io/search/autocomplete.php?query="

-- Functions to Use --

fetchHTML :: String -> IO (B.ByteString)
fetchHTML url = simpleHTTP (getRequest url) >>=
                getResponseBody >>=
                return . B.pack

getRadioInfo :: IO (Maybe RadioInfo)
getRadioInfo = decode <$> fetchHTML mainAPI

getQueue :: IO (Maybe [[String]])
getQueue = decode <$> B.drop 3 <$> fetchHTML queueAPI

getLPlay :: IO (Maybe [(Int, String, String, String)])
getLPlay = decode <$> fetchHTML lPlayAPI

getSearchPage :: String -> Int -> IO (Maybe SearchPage)
getSearchPage query pageNumber = (decode <$> fetchHTML url)
  where url = searchAPI ++ query ++ "&page=" ++ show pageNumber

getSearch :: String -> IO [Maybe SearchPage]
getSearch query = do
  firstPage <- getSearchPage query 1
  case firstPage of
    Just firstPage -> if maxPages <= 1
                      then return [Just firstPage] 
                      else (mapM (getSearchPage query) [2..maxPages]) >>=
                           return . (++) [Just firstPage]
      where maxPages = pages firstPage
    Nothing -> do return [Nothing]

requestSongId :: Int -> IO (String)
requestSongId songid = response
  where response = simpleHTTP (postRequestWithBody
                               requestAPI ""
                               ("songid=" ++ show songid)) >>= getResponseBody
                   
getAutoComplete :: String -> IO (Maybe [String])
getAutoComplete query = if length query < 2
                        then return $ Nothing
                        else decode <$> (B.init . B.init . B.drop 1) <$>
                             fetchHTML (autoCompleteAPI ++ query) 
