{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module XContest2
  ( xContestFlights
  , isTag
  , isHeaderRow
  , isDataRow
  , pageFlights
  , flightsTableRows
  , getFlightsAt
  , getLongestFlightsAt
  , getRecentFlightsAt
  , flightsFromHtml
  , cellAircraft
  , cellAircraftName
  , cellAircraftType
  , cellDate
  , cellDistance
  , cellPilot
  , cellSiteName
  , cellUrl
  , hasAttribute
  ) where

import Control.Lens
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Data.Either.Combinators    (maybeToRight)
import Data.HashMap.Strict        (HashMap)
import Data.Maybe                 (isJust)
import Data.Text                  (Text)
import Data.Text.Lens             (unpacked)
import Data.Time                  (Day)
import Network.HTTP.Req           (MonadHttp, Option, Scheme (Https))
import Text.HTML.TagSoup.Navigate
import Text.Pretty.Simple         (pShowNoColor)
import Text.URI                   (URI)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as L
import qualified Data.Text.Lazy.IO   as TLIO

import Flights
import XContest.Browse
import XContest.Parse

data FlightsTableColumns = FlightsTableColumns
  { _ftcPilot    :: Int -- pilot
  , _ftcSite     :: Int -- launch
  , _ftcDistance :: Int -- length
  , _ftcAircraft :: Int -- glider
  , _ftcDate     :: Int -- start time
  , _ftcUrl      :: Int -- info
  }
makeLenses ''FlightsTableColumns

data FlightScrapeError
  = HtmlParseFailed
  | PageHadMultipleHtmlTrees
  | FlightTableNotFound
  | ElementNotFound Text
  | NoRowsFoundInFlightsTable
  | HeadingNotFound Text
  | CellNotFoundAtIndexInRow Int (TagTree Text)
  | NoPilotName (TagTree Text)
  | NoSiteName (TagTree Text)
  | NoDistance (TagTree Text)
  | NoDate (TagTree Text)
  | NoUrl (TagTree Text)
  | InvalidUrl Text
  deriving (Show)

type FlightScrape = Either FlightScrapeError

pageFlights :: [TagTree Text] -> FlightScrape [Flight]
pageFlights = pageFlights' . filter (isTag "html")
  where
    pageFlights' [] = _Left # HtmlParseFailed
    pageFlights' [page] =
      pageFlightsTable page >>= flightsTableFlights
    pageFlights' _ = _Left # PageHadMultipleHtmlTrees

-- Find <table class="flights">
pageFlightsTable :: TagTree Text -> FlightScrape (TagTree Text)
pageFlightsTable =
  maybeToRight FlightTableNotFound
  . preview (deep (filtered (\t -> isTag "table" t && hasAttribute ("class", "flights") t)))

isTag :: Text -> TagTree Text -> Bool
isTag tagType = (== tagType) . view _TagBranch_

hasAttribute :: (Text, Text) -> TagTree Text -> Bool
hasAttribute attr t =
  isJust $ t ^? _TagBranchAttributes_ . filtered (== attr ^. attribute)

containsAnyTag :: (TagTree Text -> Bool) -> TagTree Text -> Bool
containsAnyTag test =
  isJust . preview (_TagBranchChildrenList_ . each . filtered test)

-- | Test whether a tree starts with a <tr>, with any of its children being a <td>.
-- Test with 'parseTree ("<tr><th>foo</th></tr><tr><td>bar</td></tr>" :: Text) ^.. each . filtered isDataRow',
-- which should give only the second row as a result (the first is a header row).
isDataRow :: TagTree Text -> Bool
isDataRow t = isTag "tr" t && containsAnyTag (isTag "td") t

isHeaderRow :: TagTree Text -> Bool
isHeaderRow t = isTag "tr" t && containsAnyTag (isTag "th") t

flightsTableRows :: TagTree Text -> FlightScrape [TagTree Text]
flightsTableRows flightsTable =
  case flightsTable ^.. _TagTree . deep (filtered isDataRow) of
    [] -> _Left # NoRowsFoundInFlightsTable
    rs -> _Right # rs

flightsTableFlights :: TagTree Text -> FlightScrape [Flight]
flightsTableFlights flightsTable = do
  columns <- flightsTableColumns flightsTable
  rows <- flightsTableRows flightsTable
  traverse (flightsTableRowFlight columns) rows

{-
  <td>
    <div class="full">
      <span class="cic" style="background-image:url(https://s.xcontest.net/img/flag/au.gif);" title="Australia">AU</span>
      <a class="plt" href="/world/en/pilots/detail:Esquillaro">
        Rafael Esquillaro
      </a>
    </div>
  </td>

or

  <td>
    <div class="full">
      <span class="cic" style="background-image:url(https://s.xcontest.net/img/flag/au.gif);" title="Australia">AU</span>
      <span class="plt">Phil Robinson</span>
    </div>
  </td>
-}
cellPilot :: TagTree Text -> FlightScrape Pilot
cellPilot t =
  let mname = t ^? deep (filtered (hasAttribute ("class", "plt")))
                . deep _TagText
                . to T.strip
                . filtered (/= "")
  in case mname of
    Nothing   -> Left $ NoPilotName t
    Just ""   -> Left $ NoPilotName t
    Just name -> Right . mkPilot $ name

{-
  <td>
    <div class="full">
      <span class="cic" style="background-image:url(https://s.xcontest.net/img/flag/au.gif)" title="Australia">
        AU
      </span>
      <a class="lau" href="/world/en/flights-search/?filter[point]=149.37582 -35.10363&amp;list[sort]=pts&amp;list[dir]=down">
        Lake George
      </a>
      <span class="lau" style="color:green" title="registered takeoff">✔</span>
    </div>
  </td>
-}
cellSiteName :: TagTree Text -> FlightScrape SiteName
cellSiteName t =
  case t ^? deep (filtered (isTag "a")) . deep _TagText . to T.strip of
    Nothing   -> Left $ NoSiteName t
    Just ""   -> Left $ NoSiteName t
    Just site -> Right . SiteName $ site

-- <td class="km"><strong>183.23</strong> km</td>
cellDistance :: TagTree Text -> FlightScrape Distance
cellDistance t =
  maybeToRight (NoDistance t) $
    t ^? deep (filtered (isTag "strong"))
      . deep _TagText
      . to T.strip
      . unpacked
      . _Show

{-
  <td class="cat-B">
    <div title="NIVIUK Icepeak 7 Pro" class="sponsor niviuk">
      <span class="hide">
        B
      </span>
    </div>
  </td>

or

  <td class="cat- ">
    <div title="MOYES RS3.5" class="hg">
      <span>HG</span>
      <span class="hide">FAI-1 HG</span>
    </div>
  </td>
-}
cellAircraft :: TagTree Text -> Aircraft
cellAircraft t = Aircraft (cellAircraftType t) (cellAircraftName t)

cellAircraftName :: TagTree Text -> Maybe AircraftName
cellAircraftName t =
  t ^? _TagBranchChildren_
    . _TagBranchAttributes_
    . filtered (\attr -> attr ^. attributeName == "title")
    . attributeValue
    . to mkAircraftName

cellAircraftType :: TagTree Text -> AircraftType
cellAircraftType t = xcAircraftType $
  t ^? _TagBranchChildren_
    . _TagBranchAttributes_
    . filtered (\attr -> attr ^. attributeName == "class")
    . attributeValue
  where
    xcAircraftType :: Maybe Text -> AircraftType
    xcAircraftType Nothing = Paraglider
    xcAircraftType (Just t') =
      case T.unpack t' of
        ('h':'g':_) -> HangGlider
        _           -> Paraglider

{-
  <td title="submitted: 05.01. 06:20 UTC">
    <div class="full">
      04.01.17 <em>11:05</em>
      <span class="XCutcOffset">
        UTC+11:00
      </span>
    </div>
  </td>
-}
cellDate :: TagTree Text -> FlightScrape Day
cellDate t = maybeToRight (NoDate t) $
  t ^? _TagBranchChildren_ -- div
    . indexing _TagBranchChildren_
    . index 0
    . _TagText
    . xContestDay

{-
  <td>
    <div>
      <a class="detail" title="flight detail" href="/world/en/flights/detail:Esquillaro/4.1.2017/00:05">
        <span class="hide">flight detail</span>
      </a>
    </div>
  </td>
-}
cellUrl :: TagTree Text -> FlightScrape URI
cellUrl t =
  let murl = t ^? deep (filtered (isTag "a"))
               . _TagBranchAttributes_
               . filtered (\attr -> attr ^. attributeName == "href")
               . attributeValue
  in case murl of
    Nothing -> Left (NoUrl t)
    Just url -> maybeToRight (InvalidUrl url) $
       makeAbsoluteUrlOf xContestOrg url

flightsTableRowFlight :: FlightsTableColumns -> TagTree Text -> FlightScrape Flight
flightsTableRowFlight columns row = do
  _flightPilot    <- getRowCell ftcPilot    >>= cellPilot
  _flightSiteName <- getRowCell ftcSite     >>= cellSiteName
  _flightDistance <- getRowCell ftcDistance >>= cellDistance
  _flightAircraft <- getRowCell ftcAircraft <&> cellAircraft
  _flightDate     <- getRowCell ftcDate     >>= cellDate
  _flightUrl      <- getRowCell ftcUrl      >>= cellUrl
  pure $ Flight{..}
  where
    getRowCell :: Lens' FlightsTableColumns Int -> FlightScrape (TagTree Text)
    getRowCell col =
      let idx = columns ^. col
      in (row ^?
           indexing _TagBranchChildren_
           . filtered (isTag "td")
           . index idx)
         & maybeToRight (CellNotFoundAtIndexInRow idx row)

-- Header rows look like this:
{-
<tr>
  <!--<th I18N:TRANSLATE="string:show">Zobrazit</th>-->
  <th>No.</th>
  <th><a href="..." title="order by this column...">start time</a></th>
  <th><a href="..." title="order by this column...">pilot</a></th>
  <th><a href="..." title="order by this column...">launch</a></th>
  <th><a href="..." title="order by this column...">route</a></th>
  <th><a href="..." title="order by this column...">length</a></th>
  <th><a href="..." title="order by this column..." class="down">points</a></th>
  <th><a href="..." title="order by this column...">glider</a></th>
  <th colspan="2">info</th>
</tr>
-}
flightsTableColumns :: TagTree Text -> FlightScrape FlightsTableColumns
flightsTableColumns t =
  let columnMap :: HashMap Text Int
      columnMap = t ^..
        (deep (filtered isHeaderRow)
          . indexing (deep (filtered (isTag "th")))
          <. deep _TagText
        ) . withIndex . swapped
        & HM.fromList

      lookupColumn :: Text -> FlightScrape Int
      lookupColumn headingText =
        HM.lookup headingText columnMap
        & maybeToRight (HeadingNotFound headingText)
  in do
    _ftcPilot <- lookupColumn "pilot"
    _ftcSite <- lookupColumn "launch"
    _ftcDistance <- lookupColumn "length"
    _ftcAircraft <- lookupColumn "glider"
    _ftcDate <- lookupColumn "start time"
    _ftcUrl <- lookupColumn "info" <&> (+1) -- hack: the info column has colspan=2
    pure FlightsTableColumns{..}

getFlightsAt :: MonadHttp m => Text -> Int -> Site -> m [Flight]
getFlightsAt sort num site =
  getFlightsHtmlAt sort site >>= flightsFromHtml <&> take num

getLongestFlightsAt :: MonadHttp m => Site -> Int -> m [Flight]
getLongestFlightsAt site num =
  getLongestFlightsHtmlAt site >>= flightsFromHtml <&> take num

getRecentFlightsAt :: MonadHttp m => Site -> Int -> m [Flight]
getRecentFlightsAt site num =
  getRecentFlightsHtmlAt site >>= flightsFromHtml <&> take num

xContestFlights :: MonadHttp m => Option 'Https -> m [Flight]
xContestFlights opts = xContestFlightsHtml opts >>= flightsFromHtml

flightsFromHtml :: MonadIO m => L.Text -> m [Flight]
flightsFromHtml t =
  -- t & parseTree <&> fromTagTree <&> evalTagTreePosState findFlights
  case t & L.toStrict & parseTree & pageFlights of
    Left err -> do
      liftIO . TLIO.putStrLn $ pShowNoColor err -- TODO: fail instead of just printing
      pure []
    Right flights -> pure flights
