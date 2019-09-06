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

data Crumb = Crumb
  { crumbUrl :: String
  , crumbTitle :: String
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
    let ctx = crumbsContext ["index.md"] <> metadataContext
    compile $ withDefaultTemplate ctx

  match "Activities/*" $ do
    route cleanRoute
    let ctx = crumbsContext ["index.md", "Activities.md"] <> metadataContext
    compile $ withDefaultTemplate ctx

  match "Articles/*" $ do
    route cleanRoute
    compile $ do
      let ctx = crumbsContext ["index.md", "Articles.md"] <> metadataContext
      contentCompiler
        >>= applyTemplateAndFixUrls defaultTemplate ctx

  match "Articles.md" $ do
    route cleanRoute
    compile $ do
      let articlesContext =
            crumbsContext ["index.md"]
            <> listField "articles" metadataContext (loadAll "Articles/*")
            <> metadataContext
      contentCompiler
        >>= loadAndApplyTemplate "templates/articles.html" articlesContext
        >>= loadAndApplyTemplate "templates/default.html" articlesContext
        >>= relativizeUrls
        >>= cleanIndexUrls

  match "Flying-ACT/*" $ do
    route cleanRoute
    let ctx = crumbsContext ["index.md", "Flying-ACT.md"] <> metadataContext
    compile $ withDefaultTemplate ctx

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
  contentCompiler >>= applyTemplateAndFixUrls templatePath ctx

applyTemplateAndFixUrls :: Identifier -> Context String -> Item String -> Compiler (Item String)
applyTemplateAndFixUrls templatePath ctx item =
  item
    &   loadAndApplyTemplate templatePath ctx
    >>= relativizeUrls
    >>= cleanIndexUrls

contentCompiler :: Compiler (Item String)
contentCompiler = pandocCompiler >>= saveSnapshot "content"

metadataContext :: Context String
metadataContext = metadataField <> defaultContext

crumbsContext :: [Identifier] -> Context String
crumbsContext path =
  listField "crumbs" metadataContext (flip loadSnapshot "content" `traverse` path)

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
