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
  , homeHistory
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
  -- , infoFAQ
  , infoAbout
  , Advice(..)
  , advicePage
  , advicePages
  , Stories(..)
  , storiesPage
  , storiesPages
  , Site(..)
  , siteHome
  , siteNow
  , siteFuture
  , sitePast
  , siteInfo
  , siteAdvice
  , siteStories
  , addTableClassToTables
  ) where

import Control.Lens                 (ASetter', Prism', Traversal', at,
                                     makeLenses, only, prism', re, toListOf,
                                     view)
import Control.Lens.Operators       hiding ((.=))
import Control.Lens.Plated          (deep)
import Control.Monad                (void)
import Data.Aeson                   (FromJSON, ToJSON, Value (..), object,
                                     toJSON, (.=))
import Data.Aeson.Generic.Shorthand (CamelFields, GenericToFromJSON (..))
import Data.Aeson.Lens              (key, _Object, _String)
import Data.Binary.Instances.Time   ()
import Data.Foldable                (traverse_)
import Data.List.NonEmpty           (NonEmpty (..), nonEmpty)
import Data.Maybe                   (isJust)
import Data.Text                    (Text)
import Data.Time                    (Day, getZonedTime, localDay,
                                     zonedTimeToLocalTime)
import Development.Shake            (Action, FilePattern, Lint (..),
                                     ShakeOptions (..), Verbosity (Verbose),
                                     copyFileChanged, forP, getDirectoryFiles,
                                     liftIO, readFile', shakeOptions,
                                     writeFile')
import Development.Shake.Classes    (Binary)
import Development.Shake.FilePath   (dropDirectory1, dropExtension, (</>))
import Development.Shake.Forward    (cacheAction)
import GHC.Generics                 (Generic)
import Slick                        (compileTemplate', convert, slickWithOpts)
import Slick.Pandoc                 (defaultHtml5Options,
                                     defaultMarkdownOptions,
                                     markdownToHTMLWithOpts)
import System.FilePath              (takeBaseName)
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
  , _homeFeatures :: [Page] -- ^ Featured at the top
  , _homeHistory  :: [Page] -- ^ Listed at the bottom
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
  { _infoPage             :: Page
  , _infoAbout            :: About
  , _infoSites            :: Page
  , _infoSiteRecords      :: Page
  , _infoWeatherResources :: Page
  -- , _infoFAQ              :: Page
  } deriving (Generic, Eq, Ord, Show, Binary)
  deriving (ToJSON, FromJSON) via (GenericToFromJSON '[CamelFields] Info)
makeLenses ''Info

data Advice = Advice
  { _advicePage  :: Page
  , _advicePages :: [Page]
  } deriving (Generic, Eq, Ord, Show, Binary)
  deriving (ToJSON, FromJSON) via (GenericToFromJSON '[CamelFields] Advice)
makeLenses ''Advice

data Stories = Stories
  { _storiesPage  :: Page
  , _storiesPages :: [Page]
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
      Just events -> BranchItem _elPage (toMenuItem <$> events <> ((_elPage & pageTitle .~ "All Events") :| []))

instance ToMenuItem Info where
  toMenuItem Info{..} =
    BranchItem (_infoPage & pageTitle .~ "Info") $ NE.fromList
      [ toMenuItem _infoPage
      , toMenuItem _infoSites
      , toMenuItem _infoSiteRecords
      , toMenuItem _infoWeatherResources
      -- , toMenuItem _infoFAQ
      , toMenuItem _infoAbout
      ]

instance ToMenuItem About where
  toMenuItem = LeafItem . _aboutPage

instance ToMenuItem Advice where
  toMenuItem Advice{..} =
    pageListMenuItem _advicePage _advicePages

instance ToMenuItem Stories where
  toMenuItem Stories{..} =
    pageListMenuItem _storiesPage _storiesPages

makeMenu :: Site -> Menu
makeMenu Site{..} =
  Menu . NE.fromList $
    [ toMenuItem _siteHome
    , toMenuItem _siteInfo
    , toMenuItem _siteNow
    , toMenuItem _siteFuture
    , toMenuItem _sitePast
    , toMenuItem _siteStories
    , toMenuItem _siteAdvice
    ]

setTextValue :: Text -> Text -> Value -> Value
setTextValue k t = _Object . (at k) ?~ String t

copyStaticFiles :: Action ()
copyStaticFiles = do
  filePaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*", "fonts//*", "files//*"]
  void $ forP filePaths $ \filePath ->
    copyFileChanged ("site" </> filePath) (outputFolder </> filePath)

addScrapedContent :: Action (Value -> Value)
addScrapedContent = do
  scrapedFiles <- getDirectoryFiles "." ["site/scraped//*.html"]
  foldr (.) id <$> traverse addScraped scrapedFiles
  where
    addScraped :: FilePath -> Action (Value -> Value)
    addScraped file = file
      & readFile'
      <&> T.pack
      <&> addTableClassToTables
      <&> setTextValue (T.pack $ takeBaseName file)

--------------------------------------------------------------------------------
addTableClassToTables :: Text -> Text
addTableClassToTables t =
  "<div class='table-responsive'>"
  <> (t & htmlChunks . traverse . nodeTablesClass
        ?~ "table table-striped")
  <> "</div>"

htmlChunks :: Prism' Text [Node]
htmlChunks = prism' joinNodes getNodes where
  joinNodes :: [Node] -> Text
  joinNodes nodes =
    let html' = nodes & L.toStrict . mconcat . toListOf (traverse . re html)
    in T.replace "<br></br>" "<br />" html' -- HACK to fix bug in Taggy
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
      withContact = _Object . at "contact" ?~ String "<a href='mailto:website@acthpa.org'>website@acthpa.org</a>"
      -- Add url and teaser
      fullPageData = pageData & withPageUrl & withTeaser & withContact

  -- Convert to our Page datatype
  convert fullPageData

data Tense = Future | Present | Past
  deriving (Eq, Ord, Show)

pageTense :: Day -> Page -> Tense
pageTense today Page{..} =
  case _pageEventStart of
    Nothing -> Past
    Just start ->
      if start > today
        then Future
        else case _pageEventFinish of
          Nothing -> Past
          Just finish ->
            if finish < today
              then Past
              else Present

inFuture, inPast, inPresent :: Day -> Page -> Bool
inFuture today page =
  pageTense today page == Future
inPresent today page =
  pageTense today page == Present
inPast today page =
  pageTense today page == Past

buildHome :: Site -> Home -> Action ()
buildHome site home = do
  homeT <- compileTemplate' "site/templates/index.html"
  writeOutFileWithTemplate
    homeT site (home ^. homePage) (toJSON home) (home ^. homePage . pageContent) (key "page" . _Object . at "content")

buildEvents :: Site -> EventList -> Action ()
buildEvents site EventList{..} = do
  traverse_ (buildPageDefault site) _elEvents
  page <- buildPageList _elPage _elEvents "eventList"
  buildPageDefault site page

buildNow :: Site -> EventList -> Action ()
buildNow site EventList{..} = do
  traverse_ (buildPageDefault site) _elEvents
  page <- buildPageList _elPage _elEvents "eventList"
  buildPage "windmap" site page

buildPostListPage :: Site -> Page -> [Page] -> Action ()
buildPostListPage site page posts =
  buildPageList page posts "postList"
    >>= buildPageDefault site

buildPageList :: Page -> [Page] -> Text -> Action Page
buildPageList page listables templateName = do
  let listables' = pageRelativizeUrl page <$> listables
      listJson = object [ templateName .= toJSON listables' ]
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
        [ _infoSites
        , _infoSiteRecords
        , _infoWeatherResources
        -- , _infoFAQ
        ]
  buildAbout site _infoAbout pages
  traverse_ (buildPageDefault site) pages
  buildPostListPage site _infoPage (_infoAbout ^. aboutPage : pages)

buildPageDefault :: Site -> Page -> Action ()
buildPageDefault = buildPage "default"

buildPage :: Text -> Site -> Page -> Action ()
buildPage templateName site page@Page{..} = do
  template <- compileTemplate' $ "site/templates/" <> T.unpack templateName <> ".html"
  writeOutFileWithTemplate template site page (toJSON page) (page ^. pageContent) (_Object . at "content")

-- writeOutFileWithTemplate :: ToJSON a => Template -> Site -> Page -> Lens' a Text -> a -> Action ()
-- writeOutFileWithTemplate template site page lContent obj = do
--   liftIO . putStrLn $ "Writing page: " <> pageFilePath page
--   withScrapedContent <- addScrapedContent
--   let relPath = pageFilePath page
--       addToVars =
--         addMenu site page     -- Plus the page menu
--         . withScrapedContent    -- Plus scraped content
--         . setTextValue "root" (T.pack $ pathToRootPath relPath)
--       templateVars = toJSON obj & addToVars -- The page and metadata
--   -- Render the content, expanding any mustache within the markdown
--   renderedContent <- substituteInContent templateVars (obj ^. lContent)
--   let templateVars' = obj
--         & lContent .~ addTableClassToTables renderedContent
--         & toJSON
--         & addToVars
--   -- Render the whole page
--   content <- substitute' template templateVars'
--   writeFile' (outputFolder </> relPath) (T.unpack content)

writeOutFileWithTemplate :: Template -> Site -> Page -> Value -> Text -> ASetter' Value (Maybe Value) -> Action ()
writeOutFileWithTemplate template site page vars content pContent = do
  liftIO . putStrLn $ "Writing page: " <> pageFilePath page
  withScrapedContent <- addScrapedContent
  let relPath = pageFilePath page
      vars' = vars
        & addMenu site page
        & withScrapedContent
        & setTextValue "root" (T.pack $ pathToRootPath relPath)
  content' <- substituteInContent vars' content
  let content'' = addTableClassToTables content'
      vars'' = vars' & pContent ?~ String content''
  rendered <- substitute' template vars''
  writeFile' (outputFolder </> relPath) (T.unpack rendered)

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
  buildPostListPage site _advicePage _advicePages
  traverse_ (buildPageDefault site) _advicePages

buildStories :: Site -> Stories -> Action ()
buildStories site Stories{..} = do
  buildPostListPage site _storiesPage _storiesPages
  traverse_ (buildPageDefault site) _storiesPages

buildSite :: Site -> Action ()
buildSite site@Site{..} = do
  buildHome site _siteHome
  buildNow site _siteNow
  buildEvents site _siteFuture
  buildEvents site _sitePast
  buildInfo site _siteInfo
  buildAdvice site _siteAdvice
  buildStories site _siteStories

loadSortedPages :: Day -> FilePattern -> Action [Page]
loadSortedPages today filePattern = do
  pagePaths <- getDirectoryFiles "." [filePattern]
  forP pagePaths loadPage <&> sortPages today

chooseFeatures :: Day -> [Page] -> [Page]
chooseFeatures today =
  sortPages today . filter (isJust . view pageTeaser)

buildRules :: Action ()
buildRules = do
  -- Resources
  copyStaticFiles

  -- Events (Now, Future, Past)
  today         <- liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime
  nowPage       <- loadPage "site/now.md"
  futurePage    <- loadPage "site/future.md"
  pastPage      <- loadPage "site/past.md"
  allEventPages <- loadSortedPages today "site/events//*.md"
  let futureEvents  = filter (inFuture today) allEventPages
      pastEvents    = filter (inPast today) allEventPages
      currentEvents = filter (inPresent today) allEventPages
      now    = EventList nowPage currentEvents
      future = EventList futurePage futureEvents
      past   = EventList pastPage pastEvents

  -- Advice
  advicePage'     <- loadPage "site/advice.md"
  advicePages'    <- loadSortedPages today "site/advice//*.md"
  let advice = Advice advicePage' advicePages'

  -- Stories
  storiesPage'     <- loadPage "site/stories.md"
  storiesPages'    <- loadSortedPages today "site/stories//*.md"
  let stories = Stories storiesPage' storiesPages'

  -- Home
  homePage' <- loadPage "site/index.md"
  let home = Home homePage'
        -- Feature all current events (there's probably at most 1), 4 future events and 4 news items
        -- Order them by how close to the present they are.
        (chooseFeatures today
          $ currentEvents
          <> take 4 futureEvents
          <> take 4 pastEvents)
        (chooseFeatures today
          $ allEventPages
          <> advicePages'
          <> storiesPages')

  -- Info
  infoPage'            <- loadPage "site/info.md"
  lifeMemberPages      <- loadSortedPages today "site/info/about//*.md"
  about                <- loadPage "site/info/about.md"
  -- faqPage              <- loadPage "site/info/faq.md"
  sitesPage            <- loadPage "site/info/sites.md"
  siteRecordsPage      <- loadPage "site/info/site-records.md"
  weatherResourcesPage <- loadPage "site/info/weather-resources.md"
  let info = Info
        infoPage'
        (About about lifeMemberPages)
        sitesPage
        siteRecordsPage
        weatherResourcesPage
        -- faqPage

  buildSite $ Site home now future past info advice stories

main :: IO ()
main =
  let opts = shakeOptions
        { shakeVerbosity = Verbose
        , shakeLint = Just LintFSATrace
        , shakeLintInside = ["site"]
        }
  in slickWithOpts opts buildRules
