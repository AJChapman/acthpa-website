{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Leonardo
  ( getLongestFlightsAt
  , getRecentFlightsAt
  , flightsFromHtml
  , flightFromFlightRow
  , cellPilotName
  , cellFlightKms
  , cellSiteName
  , cellAircraftName
  , cellAircraftType
  , cellFlightDate
  , rowFlightUrl
  ) where

import Control.Lens           (Fold, Prism', Traversal', at, from, ix, only, Traversal',
                               prism, to, _Show, _Wrapped)
import Control.Lens.Operators
-- import Control.Monad.IO.Class   (liftIO)
import Data.Maybe               (mapMaybe)
import Data.Text                (Text)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding  (decodeUtf8With)
import Data.Text.Lens           (unpacked)
import Data.Time                (Day, defaultTimeLocale, formatTime, parseTimeM)
import Network.HTTP.Req         (GET (..), MonadHttp, NoReqBody (..),
                                 Scheme (Https), Url, header, https,
                                 lbsResponse, req, responseBody, (/:))
import Text.Taggy.Lens          (Element, allAttributed, allNamed, attr,
                                 attributed, attrs, children, contents,
                                 elements, html, named)

import qualified Data.Text as T
-- import qualified Data.Text.IO   as TIO
import qualified Data.Text.Lazy as L

import Flights

-- Urls are like this:
--   ACTHPA (club:0.15), sorted by date (the default)
--   https://www.paraglidingforum.com/leonardo/tracks/AU/alltimes/brand:all,cat:0,class:all,xctype:all,club:0.15,pilot:0_0,takeoff:all
--
--   Sorted by XC score (&sortOrder=FLIGHT_POINTS)
--   https://www.paraglidingforum.com/leonardo/tracks/AU/alltimes/brand:all,cat:0,class:all,xctype:all,club:0.15,pilot:0_0,takeoff:all&sortOrder=FLIGHT_POINTS
--   Other filters are:
--     - LINEAR_DISTANCE
--     - FLIGHT_KM
--     - DURATION
--     - DATE
--
--   Paraglider (cat:1)
--   https://www.paraglidingforum.com/leonardo/tracks/AU/alltimes/brand:all,cat:1,class:all,xctype:all,club:0.15,pilot:0_0,takeoff:all
--
--   Hang Glider (cat:2)
--   https://www.paraglidingforum.com/leonardo/tracks/AU/alltimes/brand:all,cat:2,class:all,xctype:all,club:0.15,pilot:0_0,takeoff:all
--
--   Paraglider filtered to Manilla (takeoff:8836), Paraglider (cat:1), Sports class (class:1)
--   https://www.paraglidingforum.com/leonardo/tracks/AU/alltimes/brand:all,cat:1,class:1,xctype:all,club:all,pilot:0_0,takeoff:8836

paraglidingForumCom :: Text
paraglidingForumCom = "www.paraglidingforum.com"

leonardoFlightSearchUrl :: [Text] -> Text -> Url 'Https
leonardoFlightSearchUrl args extra =
  https paraglidingForumCom /: "leonardo" /: "tracks" /: "AU" /: "alltimes" /: T.intercalate "," args <> extra

filterSite :: Site -> Text
filterSite site =
  case site ^. siteLeonardoId of
    Unknown         -> ""
    (SiteId siteId) -> "takeoff:" <> T.pack (show siteId)
    (ClubId clubId) -> "club:" <> clubId

leonardoFlights :: MonadHttp m => [Text] -> Maybe Text -> m [Flight]
leonardoFlights args sort = do
  let extra = case sort of
        Nothing    -> ""
        Just sort' -> "&sortOrder=" <> sort'
      url = leonardoFlightSearchUrl args extra
      -- Response returns zero without a user agent the site likes. This one works.
      opts = header "User-Agent" "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.108 Safari/537.36"
  r <- req GET url NoReqBody lbsResponse opts
  let page = r & responseBody & decodeUtf8With lenientDecode
  -- liftIO $ TIO.writeFile "result.html" (L.toStrict page)
  page & flightsFromHtml & pure

flightsFromHtml :: L.Text -> [Flight]
flightsFromHtml t = t ^.. html
  . allNamed (only "table")
  . allAttributed (ix "class" . only "listTable")
  . allNamed (only "tr")
  & mapMaybe flightFromFlightRow

-- Rows look something like this:
-- <tr class="l_row1  newDate " id="row_2445723">
--   <td class="indexCell"><div>1</div>&nbsp;</td>
--   <td class="dateString" valign="top"><div>08/12/2019</div>&nbsp;</td>
--   <td class="pilotTakeoffCell" colspan="2">
--     <div id="p_2" class="pilotLink">
--       <a href="javascript:pilotTip.newTip('inline', 0, 13, 'p_2', 250, '0_71386','<img class=\'fl sprite-au\' src=\'/leonardo/img/space.gif\'  border=\'0\'  title=\'Australia\'  alt=\'Australia\'   align=\'absmiddle\' >Pete Cohn' )" onmouseout="pilotTip.hide()">
--         <img class="fl sprite-au" src="/leonardo/img/space.gif" title="Australia" alt="Australia" border="0" align="absmiddle">
--         Pete Cohn
--       </a>
--     </div>
--     <div id="at_2" class="takeoffLink">
--       <a id="t_2" href="javascript:takeoffTip.newTip('inline',-25, 13,'t_2', 250, '13734','Lake George Nth Laun... - AU')" onmouseout="takeoffTip.hide()">
--         Lake George Nth Laun... - AU
--       </a>
--     </div>
--   </td>
--   <td>0:02</td>
--   <td class="distance">0.8&nbsp;km</td>
--   <td class="distance">1.2&nbsp;km</td>
--   <td class="OLCScore" nowrap="">1.73&nbsp;<img class="icons1 sprite-icon_turnpoints" src="/leonardo/img/space.gif" title="Free Flight" alt="Free Flight" width="16" height="16" border="0" align="top"></td>
--   <td><div class="catInfo"><img class="icons1 catListIcon sprite-icon_cat_1" src="/leonardo/img/space.gif" title="Paraglider - Sport" alt="Paraglider - Sport" border="0" align="top"><div class="categoryListIconDiv"><img class="icons1 sprite-icon_class_1" src="/leonardo/img/space.gif" title="Paraglider - Sport" alt="Paraglider - Sport" border="0" align="top"></div></div></td>
-- 	<td><div align="center"><img class="brands sprite-019" src="/leonardo/img/space.gif" title="Ozone Swift 4" alt="Ozone Swift 4" border="0" align="absmiddle"></div></td>
-- 	<td valign="top" align="left"><div class="smallInfo"><a class="flightLink" id="tpa3_2445723" href="/leonardo/flight/2445723"><img class="icons1 flightIcon sprite-icon_look" src="/leonardo/img/space.gif" title="Display" alt="Display" border="0" align="top"></a><a href="javascript:nop()" onclick="geTip.newTip('inline', -315, -5, 'ge_2', 300, '2445723' , 'english')" onmouseout="geTip.hide()"><img class="icons1 geIcon sprite-geicon" src="/leonardo/img/space.gif" title="Navigate with Google Earth" alt="Navigate with Google Earth" id="ge_2" border="0" align="top"></a><span class="preview"><a class="betterTip" id="tpa0_2445723" href="javascript:nop()"><img class="icons1 previewDiv sprite-icon_info" src="/leonardo/img/space.gif" title="Display" alt="Display" border="0" align="top"></a></span></div></td>
-- </tr>
flightFromFlightRow :: Element -> Maybe Flight
flightFromFlightRow row =
  let cells = row ^.. allNamed (only "TD")
      url = makeAbsoluteUrlOf paraglidingForumCom (row ^? rowFlightUrl)
  in Flight
    <$> cells ^? traverse . cellPilotName
    <*> cells ^? traverse . cellSiteName
    <*> (cells ^.. traverse . cellFlightKms) ^? ix 1 -- The first one is straight-line distance, this one is XC kms
    <*> (Aircraft <$> cells ^? cellAircraftType <*> Just (cells ^? cellAircraftName))
    <*> cells ^? traverse . cellFlightDate
    <*> url

cellPilotName :: Fold Element Pilot
cellPilotName =
  attributed (ix "class" . only "pilotTakeoffCell")
  . elements . named (only "div")
  . attributed (ix "class" . only "pilotLink")
  . elements . named (only "a")
  . children
  . traverse
  . contents . to Pilot

cellFlightKms :: Fold Element Distance
cellFlightKms =
  attributed (ix "class" .  only "distance")
  . contents . to crop . unpacked . _Show . to Km
  where
    -- Crop e.g. "1.2 km" to "1.2"
    crop :: Text -> Text
    crop = T.takeWhile (/= ' ') -- NB: non-breaking space!

cellSiteName :: Fold Element SiteName
cellSiteName =
  attributed (ix "class" . only "pilotTakeoffCell")
  . elements
  . allNamed (only "div")
  . attributed (ix "class" . only "takeoffLink")
  . elements . named (only "a")
  . contents
  . to SiteName

cellAircraftType :: Fold [Element] AircraftType
cellAircraftType =
  traverse
  . elements . named (only "div")
  . attributed (ix "class" . only "catInfo")
  . elements . named (only "img")
  . attr "title"
  . traverse
  . to leoAircraftType
  . traverse
  where
    leoAircraftType :: Text -> Maybe AircraftType
    leoAircraftType t =
      -- Format is e.g:
      --   - "Paraglider - "
      --   - "Paraglider - Sport"
      --   - "Flex wing FAI1 - Kingpost"
      --   - "Rigid wing FAI5 - "
      case T.unpack t of
        ('P':'a':'r':'a':'g':'l':'i':'d':'e':'r':_) -> Just Paraglider
        ('F':'l':'e':'x':_)                         -> Just HangGlider
        _                                           -> Nothing

cellAircraftName :: Traversal' [Element] AircraftName
cellAircraftName =
  ix 9
  -- This is convoluted because of unclosed img tags.
  -- Taggy expects valid xhtml.
  . elements . named (only "div")
  . elements . named(only "img")
  . elements . named(only "div")
  . elements . named(only "img")
  . elements . named(only "TD")
  . elements . named(only "div")
  . elements . named(only "img")
  . attr "title" . traverse . from _Wrapped

cellFlightDate :: Traversal' Element Day
cellFlightDate =
  attributed (ix "class" . only "dateString")
  . elements . named (only "div")
  . contents
  . leoDay

-- | Print/parse a time like "08/12/2019"
leoDay :: Prism' Text Day
leoDay = prism
  (T.pack . formatTime locale dateFmt)
  (parseTimeM True locale dateFmt . T.unpack)
  where
    dateFmt = "%d/%m/%Y"
    locale = defaultTimeLocale -- TODO: AEST?

rowFlightUrl :: Fold Element Text
rowFlightUrl =
  allNamed (only "a")
  . attributed (ix "class" . only "flightLink")
  . attrs . at "href" . traverse

getFlightsAt :: MonadHttp m => Maybe Text -> Site -> Int -> m [Flight]
getFlightsAt sort site top =
  take top <$> leonardoFlights [filterSite site] sort

getLongestFlightsAt :: MonadHttp m => Site -> Int -> m [Flight]
getLongestFlightsAt = getFlightsAt (Just "FLIGHT_KM")

getRecentFlightsAt :: MonadHttp m => Site -> Int -> m [Flight]
getRecentFlightsAt = getFlightsAt (Just "DATE")
