{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module XContest.Browse
  ( xContestOrg
  , xContestFlightSearchUrl
  , xContestOptions
  , getFlightsHtmlAt
  , getLongestFlightsHtmlAt
  , getRecentFlightsHtmlAt
  , xContestFlightsHtml
  , xContestLogin
  ) where

import Control.Category         ((>>>))
import Control.Lens.Operators
import Data.Text                (Text)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding  (decodeUtf8With)
import Network.HTTP.Client      (CookieJar)
import Network.HTTP.Req

import qualified Data.Text.Lazy as L

import Flights

xContestOrg :: Text
xContestOrg = "www.xcontest.org"

xContestWorldEnUrl :: Url 'Https
xContestWorldEnUrl =
  https xContestOrg /: "world" /: "en"

xContestFlightSearchUrl :: Url 'Https
xContestFlightSearchUrl =
  xContestWorldEnUrl /: "flights-search"

xContestOptions :: CookieJar -> Text -> Site -> Option scheme
xContestOptions cookies sort site =
  let radiusInMetres = site ^. siteRadius
  in "filter[point]"            =: formatLL (site ^. siteLocation)
    <> "filter[radius]"        =: (radiusInMetres :: Int)
    -- <> "filter[mode]"          =: ("START" :: Text)
    -- <> "filter[date_mode]"     =: ("dmy" :: Text)
    -- <> "filter[date]"          =: ("" :: Text)
    -- <> "filter[value_mode]"    =: ("dst" :: Text)
    -- <> "filter[min_value_dst]" =: ("" :: Text)
    -- <> "filter[catg]"          =: ("" :: Text)
    -- <> "filter[route_types]"   =: ("" :: Text)
    -- <> "filter[avg]"           =: ("" :: Text)
    -- <> "filter[pilot]"         =: ("" :: Text)
    <> "list[sort]"            =: sort -- pts, time_start
    <> "list[dir]"             =: ("down" :: Text)
    <> cookieJar cookies

getFlightsHtmlAt :: MonadHttp m => CookieJar -> Text -> Site -> m L.Text
getFlightsHtmlAt cookies sort site =
  xContestFlightsHtml (xContestOptions cookies sort site)

getLongestFlightsHtmlAt :: MonadHttp m => CookieJar -> Site -> m L.Text
getLongestFlightsHtmlAt cookies = getFlightsHtmlAt cookies "pts"

getRecentFlightsHtmlAt :: MonadHttp m => CookieJar -> Site -> m L.Text
getRecentFlightsHtmlAt cookies = getFlightsHtmlAt cookies "time_start"

xContestFlightsHtml :: MonadHttp m => Option 'Https -> m L.Text
xContestFlightsHtml opts =
  req GET
    xContestFlightSearchUrl
    NoReqBody
    lbsResponse
    opts
  >>=
    (responseBody
    >>> decodeUtf8With lenientDecode
    >>> pure)

xContestLogin :: MonadHttp m => Text -> Text -> m CookieJar
xContestLogin username password =
  req POST
    xContestWorldEnUrl
    (ReqBodyUrlEnc
      ( queryParam "login[username]" (Just username)
     <> queryParam "login[password]" (Just password)
     <> queryParam "login[persist_login]" (Just ("Y" :: Text))))
    lbsResponse
    mempty
  <&> responseCookieJar
