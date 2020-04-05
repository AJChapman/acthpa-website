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
  ) where

import Control.Lens.Operators
import Control.Monad.IO.Class   (liftIO)
import Data.Text                (Text)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding  (decodeUtf8With)
import Network.HTTP.Req         (GET (..), MonadHttp, NoReqBody (..), Option,
                                 Scheme (Https), Url, https, lbsResponse, req,
                                 req', responseBody, (/:), (=:))

import qualified Data.Text.IO   as TIO
import qualified Data.Text.Lazy as L

import Flights

xContestOrg :: Text
xContestOrg = "www.xcontest.org"

xContestFlightSearchUrl :: Url 'Https
xContestFlightSearchUrl =
  https xContestOrg /: "world" /: "en" /: "flights-search"

xContestOptions :: Text -> Site -> Option scheme
xContestOptions sort site =
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

getFlightsHtmlAt :: MonadHttp m => Text -> Site -> m L.Text
getFlightsHtmlAt sort site =
  xContestFlightsHtml (xContestOptions sort site)

getLongestFlightsHtmlAt :: MonadHttp m => Site -> m L.Text
getLongestFlightsHtmlAt = getFlightsHtmlAt "pts"

getRecentFlightsHtmlAt :: MonadHttp m => Site -> m L.Text
getRecentFlightsHtmlAt = getFlightsHtmlAt "time_start"

xContestFlightsHtml :: MonadHttp m => Option 'Https -> m L.Text
xContestFlightsHtml opts = do
  req' GET xContestFlightSearchUrl NoReqBody opts (\rq _ -> liftIO $ print rq)
  r <- req GET
    xContestFlightSearchUrl
    NoReqBody
    lbsResponse
    opts
  let page = r
        & responseBody
        & decodeUtf8With lenientDecode
  -- For debugging
  liftIO $ TIO.writeFile "result.html" (L.toStrict page)
  pure page

