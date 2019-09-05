--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Function   ((&))
import Data.List       (isSuffixOf)
import Hakyll
import System.FilePath (takeBaseName, takeDirectory, (</>))
--------------------------------------------------------------------------------

config :: Configuration
config = defaultConfiguration
  { deployCommand = "rsync -av _site/ farfromthere.net:acthpa.farfromthere.net/"
  }

main :: IO ()
main = hakyllWith config $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "js/*" $ do
    route idRoute
    compile copyFileCompiler

  match "fonts/**" $ do
    route idRoute
    compile copyFileCompiler

  match "templates/*" $ compile templateCompiler

  match (fromList
          [ "About.md"
          , "Activities.md"
          , "Flying-ACT.md"
          ]) $ do
    route cleanRoute
    compile $ withDefaultTemplate defaultContext

  match "Activities/*" $ do
    route cleanRoute
    compile $ withDefaultTemplate defaultContext

  match "Articles/*" $ do
    route cleanRoute
    compile $ do
      content <- pandocCompiler
      content
        &   loadAndApplyTemplate "templates/article-listing.html" metadataContext
        >>= saveSnapshot "listing"
      content
        & applyTemplateAndFixUrls defaultTemplate metadataContext

  match "Articles.md" $ do
    route cleanRoute
    compile $ do
      articles <- loadAllSnapshots "Articles/*" "listing"
      let articlesContext =
            listField "articles" metadataContext (pure articles)
            <> metadataContext
      pandocCompiler
        >>= loadAndApplyTemplate "templates/articles.html" articlesContext
        >>= loadAndApplyTemplate "templates/default.html" articlesContext
        >>= relativizeUrls
        >>= cleanIndexUrls

  match "Flying-ACT/*" $ do
    route cleanRoute
    compile $ withDefaultTemplate defaultContext

  match "features/*" $ compile $
    withTemplate "templates/feature.html" metadataContext

  match "index.md" $ do
    route (setExtension ".html")
    compile $ do
      let features = loadAll "features/*"
          indexContext =
            listField "features" metadataContext features <>
            metadataContext
      withTemplate "templates/index.html" indexContext

defaultTemplate :: Identifier
defaultTemplate = "templates/default.html"

withDefaultTemplate :: Context String -> Compiler (Item String)
withDefaultTemplate = withTemplate defaultTemplate

withTemplate :: Identifier -> Context String -> Compiler (Item String)
withTemplate templatePath ctx =
  pandocCompiler >>= applyTemplateAndFixUrls templatePath ctx

applyTemplateAndFixUrls :: Identifier -> Context String -> Item String -> Compiler (Item String)
applyTemplateAndFixUrls templatePath ctx item =
  item
    &   loadAndApplyTemplate templatePath ctx
    >>= relativizeUrls
    >>= cleanIndexUrls

metadataContext :: Context String
metadataContext = metadataField <> defaultContext

--------------------------------------------------------------------------------
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
      where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pat replacement)
  where
    pat = "/index.html"
    replacement = const ""

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "/index.html"
