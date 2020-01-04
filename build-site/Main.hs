{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main
  ( main
  , Home(..)
  , homePage
  , homeFeatures
  , Page(..)
  , pageTitle
  , pageContent
  , pageUrl
  , pageTeaser
  , pageImage
  , pageAuthor
  , pagePublished
  , pageEventStart
  , pageEventFinish
  , EventList
  , elPage
  , elEvents
  , About(..)
  , aboutPage
  , aboutLifeMembers
  , Info(..)
  , infoPage
  , infoSites
  , infoFAQ
  , infoAbout
  , Site(..)
  , siteHome
  , siteNow
  , siteFuture
  , sitePast
  , siteInfo
  ) where

import Control.Lens                  (at, ix, makeLenses)
import Control.Lens.Operators        hiding ((.=))
import Control.Monad                 (void)
import Data.Aeson                    (FromJSON, ToJSON, Value (..), object,
                                      toJSON, (.=))
import Data.Aeson.Generic.Shorthand  (CamelFields, GenericToFromJSON (..))
import Data.Aeson.Lens               (_Object, _String)
import Data.Binary.Instances.Time    ()
import Data.Text                     (Text)
import Data.Time                     (Day, getZonedTime, localDay,
                                      zonedTimeToLocalTime)
import Development.Shake             (Action, copyFileChanged, forP,
                                      getDirectoryFiles, liftIO, readFile',
                                      writeFile')
import Development.Shake.Classes     (Binary)
import Development.Shake.FilePath    (dropDirectory1, (-<.>), (</>))
import Development.Shake.Forward     (cacheAction)
import GHC.Generics                  (Generic)
import Slick                         (compileTemplate', convert, markdownToHTML,
                                      slick, substitute)
import System.FilePath               (joinPath, splitDirectories)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5              ((!))
import Text.Mustache                 (Template, checkedSubstitute,
                                      compileTemplate)
import Text.Mustache.Types           (mFromJSON)

import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as TL
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

outputFolder :: FilePath
outputFolder = "gen/"

-- | Page is the data structure that we load from MarkDown files.
-- It has a lot of `Maybe` fields, because most metadata is optional.
-- We also keep it around in other data structures (for now).
data Page = Page
  { _pageTitle       :: Text
  , _pageContent     :: Text
  , _pageUrl         :: Text
  , _pageTeaser      :: Maybe Text
  , _pageImage       :: Maybe Text
  , _pageAuthor      :: Maybe Text
  , _pagePublished   :: Maybe Day
  , _pageEventStart  :: Maybe Day
  , _pageEventFinish :: Maybe Day
  } deriving (Generic, Eq, Ord, Show, Binary)
  deriving (ToJSON, FromJSON) via (GenericToFromJSON '[CamelFields] Page)
$(makeLenses ''Page)

data Home = Home
  { _homePage     :: Page
  , _homeFeatures :: [Page]
  } deriving (Generic, Eq, Ord, Show, Binary)
  deriving (ToJSON, FromJSON) via (GenericToFromJSON '[CamelFields] Home)
$(makeLenses ''Home)

data EventList = EventList
  { _elPage   :: Page
  , _elEvents :: [Page]
  } deriving (Generic, Eq, Ord, Show, Binary)
  deriving (ToJSON, FromJSON) via (GenericToFromJSON '[CamelFields] EventList)
$(makeLenses ''EventList)

data About = About
  { _aboutPage        :: Page
  , _aboutLifeMembers :: [Page]
  } deriving (Generic, Eq, Ord, Show, Binary)
  deriving (ToJSON, FromJSON) via (GenericToFromJSON '[CamelFields] About)
makeLenses ''About

data Info = Info
  { _infoPage  :: Page
  , _infoSites :: [Page]
  , _infoFAQ   :: Page
  , _infoAbout :: About
  } deriving (Generic, Eq, Ord, Show, Binary)
  deriving (ToJSON, FromJSON) via (GenericToFromJSON '[CamelFields] Info)
makeLenses ''Info

data Site = Site
  { _siteHome   :: Home
  , _siteNow    :: EventList
  , _siteFuture :: EventList
  , _sitePast   :: EventList
  , _siteInfo   :: Info
  -- , _siteAdvice  :: Advice
  -- , _siteStories :: Stories
  } deriving (Generic, Eq, Ord, Show, Binary)
  deriving (ToJSON, FromJSON) via (GenericToFromJSON '[CamelFields] Site)

makeLenses ''Site

writeOutFileWithTemplate :: (ToJSON a) => Template -> FilePath -> a -> Action ()
writeOutFileWithTemplate template relPath obj =
  writeFile'
    (outputFolder </> relPath)
    (T.unpack $ renderWithTemplate (pathToRootPath relPath) template obj)

pathToRootPath :: FilePath -> FilePath
pathToRootPath p =
  let depth = length (splitDirectories p) - 1
  in joinPath . take depth $ repeat "../"

setTextValue :: Text -> Text -> Value -> Value
setTextValue k t = _Object . at k ?~ String t

renderWithTemplate :: (ToJSON a) => FilePath -> Template -> a -> Text
renderWithTemplate pathToRoot t d =
  substitute t (toJSON d & setTextValue "root" (T.pack pathToRoot))

copyStaticFiles :: Action ()
copyStaticFiles = do
  filePaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*", "fonts//*"]
  void $ forP filePaths $ \filePath ->
    copyFileChanged ("site" </> filePath) (outputFolder </> filePath)

addScrapedContent :: Action (Value -> Value)
addScrapedContent = do
  recentCanberra <- readFile' "site/scraped/recentCanberra.html"
  pure
    $ setTextValue "recentCanberra" (T.pack recentCanberra)

loadPage :: FilePath -> Action Page
loadPage srcPath = cacheAction ("build" :: Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding page: " <> srcPath

  -- Load the markdown page
  mustacheContent <- compileTemplate' srcPath

  -- Substitute mustache values within the markdown page
  withScrapedContent <- addScrapedContent
  let contentVals = withScrapedContent $ Object mempty
      content = substitute mustacheContent contentVals

  -- Convert page to HTML, with metadata as JSON blob
  pageData <- markdownToHTML content

  -- Add more metadata: (clean) url, teaser
  let url = T.pack . dropDirectory1 $ srcPath -<.> "html"
      withPageUrl = setTextValue "url" url
      teaser = pageData ^? _Object . at "teaser" . traverse . _String
  teaser' <- traverse markdownToHTML teaser
  let teaser'' = teaser' ^? traverse . _Object . ix "content"
  let withTeaser = _Object . at "teaser" .~ teaser''
  -- Add additional metadata we've been able to compute
  let fullPageData = pageData & withPageUrl & withTeaser & withScrapedContent
  convert fullPageData

data Tense = Future | Present | Past
  deriving (Eq, Ord, Show)

pageTense :: Day -> Page -> Maybe Tense
pageTense today Page{..} =
  case _pageEventStart of
    Nothing -> Nothing
    Just start ->
      if start > today
        then Just Future
        else case _pageEventFinish of
          Nothing -> Just Past
          Just finish ->
            if finish < today
              then Just Past
              else Just Present

inFuture, inPast, inPresent :: Day -> Page -> Bool
inFuture today page =
  pageTense today page == Just Future
inPresent today page =
  pageTense today page == Just Present
inPast today page =
  pageTense today page == Just Past

buildHome :: Site -> Home -> Action ()
buildHome site home = do
  homeT <- compileTemplate' "site/templates/index.html"
  let templateVars =
        toJSON home
        & addMenu site (home ^. homePage)
  writeOutFileWithTemplate homeT "index.html" templateVars

buildEvents :: FilePath -> EventList -> Action ()
buildEvents path EventList{..} = do
  eventListT <- compileTemplate' "site/templates/eventList.html"
  let eventListJson = (object [ "eventList" .= toJSON _elEvents ])
  eventList <- substitute' eventListT eventListJson
  defaultT <- compileTemplate' "site/templates/default.html"
  newContent <- substituteInContent (_elPage ^. pageContent) (object [ "eventList" .= eventList ])
  let page = _elPage & pageContent .~ newContent
  writeOutFileWithTemplate defaultT path page

substitute' :: ToJSON k => Template -> k -> Action Text
substitute' tpl v =
  let (errs, result) = checkedSubstitute tpl (mFromJSON v)
  in case errs of
    [] -> pure result
    _  -> fail $ show errs

substituteInContent :: ToJSON k => Text -> k -> Action Text
substituteInContent t v =
  let tpl = compileTemplate "" t
  in case tpl of
    Left err   -> fail $ show err
    Right tpl' -> substitute' tpl' v

addMenu :: Site -> Page -> Value -> Value
addMenu site page =
  setTextValue "menu" $ makeMenu site page

makeMenu :: Site -> Page -> Text
makeMenu site activePage =
  TL.toStrict . renderHtml . H.ul $
    H.li (pageLink (site ^. siteHome . homePage))
    <> H.li (pageLink (site ^. siteNow . elPage))
    <> H.li (pageLink (site ^. siteFuture . elPage))
    <> H.li (pageLink (site ^. sitePast . elPage))
    <> H.li
         ( pageLink (site ^. siteInfo . infoPage)
         <> H.ul
             ( H.li (pageLink (site ^. siteInfo . infoFAQ))
             <> H.li (pageLink (site ^. siteInfo . infoAbout . aboutPage))
             )
           ! A.class_ "dropdown"
         )
         ! A.class_ "has-dropdown"
  where
    pageLink :: Page -> H.Html
    pageLink p =
      let link = H.a
            (H.toHtml $ p ^. pageTitle)
            ! A.href (H.toValue $ p ^. pageUrl)
      in if p == activePage
         then link ! A.class_ "active"
         else link

buildSite :: Site -> Action ()
buildSite site@Site{..} = do
  buildHome site _siteHome
  buildEvents "future/index.html" _siteFuture
  buildEvents "past/index.html" _sitePast

selectFeatures :: [Page] -> [Page]
selectFeatures = id -- TODO: closest to now first, weighted randomisation

buildRules :: Action ()
buildRules = do
  copyStaticFiles
  eventPaths <- getDirectoryFiles "." ["site/events//*.md"]
  allEventPages <- forP eventPaths loadPage
  today <- liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime
  let futureEvents  = filter (inFuture today) allEventPages
      pastEvents    = filter (inPast today) allEventPages
      currentEvents = filter (inPresent today) allEventPages
  homePage'  <- loadPage "site/index.md"
  nowPage    <- loadPage "site/now.md"
  futurePage <- loadPage "site/future.md"
  pastPage   <- loadPage "site/past.md"
  infoPage'  <- loadPage "site/info.md"
  faqPage    <- loadPage "site/info/faq.md"
  lifeMemberPagePaths <- getDirectoryFiles "." ["site/info/about//*.md"]
  lifeMemberPages <- forP lifeMemberPagePaths loadPage
  about  <- loadPage "site/info/about.md"
  let home = Home
        homePage'
        (selectFeatures $ futureEvents <> pastEvents) -- TODO: also list of stories
      site = Site
        home
        (EventList nowPage currentEvents)
        (EventList futurePage futureEvents)
        (EventList pastPage pastEvents)
        (Info infoPage' [] faqPage (About about lifeMemberPages))
  buildSite site

main :: IO ()
main = slick buildRules
