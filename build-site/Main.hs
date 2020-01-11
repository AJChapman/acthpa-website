{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main
  ( main
  , Home(..)
  , homePage
  , homeFeatures
  , EventList
  , elPage
  , elEvents
  , About(..)
  , aboutPage
  , aboutLifeMembers
  , Info(..)
  , infoPage
  , infoSites
  , infoSiteRecords
  , infoWeatherResources
  , infoFAQ
  , infoAbout
  , Advice(..)
  , advicePage
  , adviceHowToReadWeather
  , adviceFlyingCanberraTips
  , Stories(..)
  , storiesPage
  , storiesFences
  , storiesWindTalkerMan
  , Site(..)
  , siteHome
  , siteNow
  , siteFuture
  , sitePast
  , siteInfo
  , siteAdvice
  , siteStories
  ) where

import Control.Lens                 (Lens', Prism', Traversal', at, makeLenses,
                                     only, prism', re, toListOf)
import Control.Lens.Operators       hiding ((.=))
import Control.Lens.Plated          (deep)
import Control.Monad                (void)
import Data.Aeson                   (FromJSON, ToJSON, Value (..), object,
                                     toJSON, (.=))
import Data.Aeson.Generic.Shorthand (CamelFields, GenericToFromJSON (..))
import Data.Aeson.Lens              (_Object, _String)
import Data.Binary.Instances.Time   ()
import Data.Foldable                (traverse_)
import Data.List.NonEmpty           (nonEmpty)
import Data.Text                    (Text)
import Data.Time                    (Day, getZonedTime, localDay,
                                     zonedTimeToLocalTime)
import Development.Shake            (Action, copyFileChanged, forP,
                                     getDirectoryFiles, liftIO, readFile',
                                     writeFile')
import Development.Shake.Classes    (Binary)
import Development.Shake.FilePath   (dropDirectory1, dropExtension, (</>))
import Development.Shake.Forward    (cacheAction)
import GHC.Generics                 (Generic)
import Slick                        (compileTemplate', convert, slick)
import Slick.Pandoc                 (defaultHtml5Options,
                                     defaultMarkdownOptions,
                                     markdownToHTMLWithOpts)
import System.FilePath              ((-<.>))
import Text.Mustache                (Template, checkedSubstitute,
                                     compileTemplate)
import Text.Mustache.Types          (mFromJSON)
import Text.Pandoc                  (ReaderOptions (..),
                                     githubMarkdownExtensions, pandocExtensions)
import Text.Taggy.Lens              (Node, attr, children, element, html, named)

import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as T
import qualified Data.Text.Lazy     as L

import Menu
import Page

outputFolder :: FilePath
outputFolder = "gen/"

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
  { _infoPage        :: Page
  , _infoAbout       :: About
  , _infoSites       :: Page
  , _infoSiteRecords :: Page
  , _infoWeatherResources   :: Page
  , _infoFAQ         :: Page
  } deriving (Generic, Eq, Ord, Show, Binary)
  deriving (ToJSON, FromJSON) via (GenericToFromJSON '[CamelFields] Info)
makeLenses ''Info

data Advice = Advice
  { _advicePage               :: Page
  , _adviceHowToReadWeather   :: Page
  , _adviceFlyingCanberraTips :: Page
  } deriving (Generic, Eq, Ord, Show, Binary)
  deriving (ToJSON, FromJSON) via (GenericToFromJSON '[CamelFields] Advice)
makeLenses ''Advice

data Stories = Stories
  { _storiesPage          :: Page
  , _storiesFences        :: Page
  , _storiesWindTalkerMan :: Page
  } deriving (Generic, Eq, Ord, Show, Binary)
  deriving (ToJSON, FromJSON) via (GenericToFromJSON '[CamelFields] Stories)
makeLenses ''Stories

data Site = Site
  { _siteHome    :: Home
  , _siteNow     :: EventList
  , _siteFuture  :: EventList
  , _sitePast    :: EventList
  , _siteInfo    :: Info
  , _siteAdvice  :: Advice
  , _siteStories :: Stories
  } deriving (Generic, Eq, Ord, Show, Binary)
  deriving (ToJSON, FromJSON) via (GenericToFromJSON '[CamelFields] Site)
makeLenses ''Site

instance ToMenuItem Home where toMenuItem = LeafItem . _homePage

instance ToMenuItem EventList where
  toMenuItem EventList{..} =
    case nonEmpty _elEvents of
      Nothing     -> LeafItem _elPage
      Just events -> BranchItem _elPage (toMenuItem <$> events)

instance ToMenuItem Info where
  toMenuItem Info{..} =
    BranchItem _infoPage $ NE.fromList
      [ toMenuItem _infoAbout
      , toMenuItem _infoSites
      , toMenuItem _infoSiteRecords
      , toMenuItem _infoWeatherResources
      , toMenuItem _infoFAQ
      ]

instance ToMenuItem About where
  toMenuItem = LeafItem . _aboutPage

instance ToMenuItem Advice where
  toMenuItem Advice{..} =
    BranchItem _advicePage $ NE.fromList
      [ toMenuItem _adviceHowToReadWeather
      , toMenuItem _adviceFlyingCanberraTips
      ]

instance ToMenuItem Stories where
  toMenuItem Stories{..} =
    BranchItem _storiesPage $ NE.fromList
      [ toMenuItem _storiesFences
      , toMenuItem _storiesWindTalkerMan
      ]

makeMenu :: Site -> Menu
makeMenu Site{..} =
  Menu . NE.fromList $
    [ toMenuItem _siteHome
    , toMenuItem _siteNow
    , toMenuItem _siteFuture
    , toMenuItem _sitePast
    , toMenuItem _siteStories
    , toMenuItem _siteAdvice
    , toMenuItem _siteInfo
    ]

setTextValue :: Text -> Text -> Value -> Value
setTextValue k t = _Object . at k ?~ String t

copyStaticFiles :: Action ()
copyStaticFiles = do
  filePaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*", "fonts//*"]
  void $ forP filePaths $ \filePath ->
    copyFileChanged ("site" </> filePath) (outputFolder </> filePath)

addScrapedContent :: Action (Value -> Value)
addScrapedContent =
  foldr (.) id <$> traverse addScraped
    [ "longestCanberra"
    , "longestSpringHill"
    , "longestCollector"
    , "longestLakeGeorge"
    , "longestLanyon"
    , "longestPigHill"
    , "longestHoneysuckle"
    , "longestBowning"
    , "longestArgalong"
    , "longestCastleHill"
    , "longestBooroomba"
    , "longestCarols"
    , "recentCanberra"
    ]
  where
    addScraped :: FilePath -> Action (Value -> Value)
    addScraped filename =
      "site/scraped" </> filename -<.> "html"
        & readFile'
        <&> T.pack
        <&> addTableClassToTables
        <&> setTextValue (T.pack filename)

--------------------------------------------------------------------------------
addTableClassToTables :: Text -> Text
addTableClassToTables =
  htmlChunks . traverse . nodeTablesClass ?~ "table"

htmlChunks :: Prism' Text [Node]
htmlChunks = prism' joinNodes getNodes where
  joinNodes :: [Node] -> Text
  joinNodes = L.toStrict . mconcat . toListOf (traverse . re html)
  getNodes :: Text -> Maybe [Node]
  getNodes t = ("<html>" <> L.fromStrict t <> "</html>") ^? html . element . children

nodeTablesClass :: Traversal' Node (Maybe Text)
nodeTablesClass = nodeElementsClass "table"

nodeElementsClass :: Text -> Traversal' Node (Maybe Text)
nodeElementsClass elt =
  element
  . deep (named (only elt))
  . attr "class"
--------------------------------------------------------------------------------

loadPage :: FilePath -> Action Page
loadPage srcPath = cacheAction ("build" :: Text, srcPath) $ do
  liftIO . putStrLn $ "Loading page: " <> srcPath

  -- Load the markdown page and metadata
  markdown <- readFile' srcPath <&> T.pack
  let readerOptions = defaultMarkdownOptions { readerExtensions = githubMarkdownExtensions <> pandocExtensions }
      writerOptions = defaultHtml5Options
  pageData <- markdown & markdownToHTMLWithOpts readerOptions writerOptions

  -- Add more metadata: (clean) url, teaser
  let url = T.pack . dropDirectory1 . dropExtension $ srcPath
      url' = if url == "index" -- Special case for home page
        then ""
        else url
      withPageUrl = setTextValue "url" url'
      teaser = pageData ^? _Object . at "teaser" . traverse . _String
      withTeaser = _Object . at "teaser" .~ (String <$> teaser)
      -- Add url and teaser
      fullPageData = pageData & withPageUrl & withTeaser

  -- Convert to our Page datatype
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
  writeOutFileWithTemplate
    homeT site (home ^. homePage) (homePage . pageContent) home

buildEvents :: Site -> EventList -> Action ()
buildEvents site EventList{..} = do
  traverse_ (buildPageDefault site) _elEvents
  page <- buildPageList _elPage _elEvents "eventList"
  buildPageDefault site page

buildNow :: Site -> EventList -> Action ()
buildNow site EventList{..} = do
  traverse_ (buildPageDefault site) _elEvents
  page <- buildPageList _elPage _elEvents "eventList"
  -- TODO: add in the live wind diagram?
  buildPage "windmap" site page

buildPostListPage :: Site -> Page -> [Page] -> Action ()
buildPostListPage site page posts =
  buildPageList page posts "postList"
    >>= buildPageDefault site

buildPageList :: Page -> [Page] -> Text -> Action Page
buildPageList page listables templateName = do
  let listables' = pageRelativizeUrl page <$> listables
      listJson = (object [ templateName .= toJSON listables' ])
  listT <- compileTemplate' $ "site/templates/" <> T.unpack templateName <> ".html"
  list <- substitute' listT listJson
  page & pageContent %%~ substituteInContent (object [ templateName .= list ])

buildAbout :: Site -> About -> [Page] -> Action ()
buildAbout site About{..} pages = do
  buildPostListPage site _aboutPage pages
  traverse_ (buildPageDefault site) _aboutLifeMembers

buildInfo :: Site -> Info -> Action ()
buildInfo site Info{..} = do
  let pages =
        [ _infoFAQ
        , _infoSites
        , _infoSiteRecords
        , _infoWeatherResources
        ]
  buildAbout site _infoAbout pages
  buildPostListPage site _infoPage pages
  traverse_ (buildPageDefault site) pages

buildPageDefault :: Site -> Page -> Action ()
buildPageDefault = buildPage "default"

buildPage :: Text -> Site -> Page -> Action ()
buildPage templateName site page@Page{..} = do
  template <- compileTemplate' $ "site/templates/" <> T.unpack templateName <> ".html"
  writeOutFileWithTemplate template site page pageContent page

writeOutFileWithTemplate :: ToJSON a => Template -> Site -> Page -> Lens' a Text -> a -> Action ()
writeOutFileWithTemplate template site page lContent obj = do
  liftIO . putStrLn $ "Writing page: " <> pageFilePath page
  withScrapedContent <- addScrapedContent
  let relPath = pageFilePath page
      addToVars =
        addMenu site page     -- Plus the page menu
        . withScrapedContent    -- Plus scraped content
        . setTextValue "root" (T.pack $ pathToRootPath relPath)
      templateVars = toJSON obj & addToVars -- The page and metadata
  -- Render the content, expanding any mustache within the markdown
  renderedContent <- substituteInContent templateVars (obj ^. lContent)
  let templateVars' = obj
        & lContent .~ addTableClassToTables renderedContent
        & toJSON
        & addToVars
  -- Render the whole page
  content <- substitute' template templateVars'
  writeFile' (outputFolder </> relPath) (T.unpack content)

substitute' :: ToJSON k => Template -> k -> Action Text
substitute' tpl v = do
  let (errs, result) = checkedSubstitute tpl (mFromJSON v)
  case errs of
    [] -> pure result
    _  -> fail $ show errs

substituteInContent :: ToJSON k => k -> Text -> Action Text
substituteInContent v t =
  let tpl = compileTemplate "" t
  in case tpl of
    Left err   -> fail $ show err
    Right tpl' -> substitute' tpl' v

addMenu :: Site -> Page -> Value -> Value
addMenu site page =
  setTextValue "menu" $ makeMenu site & menuText page

buildAdvice :: Site -> Advice -> Action ()
buildAdvice site Advice{..} = do
  let pages =
        [ _adviceHowToReadWeather
        , _adviceFlyingCanberraTips
        ]
  buildPostListPage site _advicePage pages
  traverse_ (buildPageDefault site) pages

buildStories :: Site -> Stories -> Action ()
buildStories site Stories{..} = do
  let pages =
        [ _storiesFences
        , _storiesWindTalkerMan
        ]
  buildPostListPage site _storiesPage pages
  traverse_ (buildPageDefault site) pages

buildSite :: Site -> Action ()
buildSite site@Site{..} = do
  buildHome site _siteHome
  buildNow site _siteNow
  buildEvents site _siteFuture
  buildEvents site _sitePast
  buildInfo site _siteInfo
  buildAdvice site _siteAdvice
  buildStories site _siteStories

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
  homePage'              <- loadPage "site/index.md"
  nowPage                <- loadPage "site/now.md"
  futurePage             <- loadPage "site/future.md"
  pastPage               <- loadPage "site/past.md"
  infoPage'              <- loadPage "site/info.md"
  lifeMemberPagePaths    <- getDirectoryFiles "." ["site/info/about//*.md"]
  lifeMemberPages        <- forP lifeMemberPagePaths loadPage
  about                  <- loadPage "site/info/about.md"
  faqPage                <- loadPage "site/info/faq.md"
  sitesPage              <- loadPage "site/info/sites.md"
  siteRecordsPage        <- loadPage "site/info/site-records.md"
  weatherResourcesPage   <- loadPage "site/info/weather-resources.md"
  advicePage'            <- loadPage "site/advice.md"
  howToReadWeatherPage   <- loadPage "site/advice/reading-weather-canberra.md"
  flyingCanberraTipsPage <- loadPage "site/advice/flying-canberra-a-few-tips.md"

  storiesPage'      <- loadPage "site/stories.md"
  fencesPage        <- loadPage "site/stories/fences.md"
  windTalkerManPage <- loadPage "site/stories/the-windtalker-man.md"

  let home = Home
        homePage'
        (selectFeatures $ futureEvents <> pastEvents) -- TODO: also list of stories
      site = Site
        home
        (EventList nowPage currentEvents)
        (EventList futurePage futureEvents)
        (EventList pastPage pastEvents)
        (Info infoPage' (About about lifeMemberPages) sitesPage siteRecordsPage weatherResourcesPage faqPage )
        (Advice advicePage' howToReadWeatherPage flyingCanberraTipsPage)
        (Stories storiesPage' fencesPage windTalkerManPage)
  buildSite site

main :: IO ()
main = slick buildRules
