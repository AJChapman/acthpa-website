--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.List       (isSuffixOf)
import Hakyll
import System.FilePath (takeBaseName, takeDirectory, (</>))


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "templates/*" $ compile templateBodyCompiler

  match (fromList
          [ "About.md"
          , "Activities.md"
          , "Articles.md"
          , "Flying-ACT.md"
          ]) $ do
    route cleanRoute
    defaultCompile defaultContext

  match "Activities/*" $ do
    route cleanRoute
    defaultCompile defaultContext

  match "Articles/*" $ do
    route cleanRoute
    defaultCompile defaultContext

  match "Flying-ACT/*" $ do
    route cleanRoute
    defaultCompile defaultContext

  match "index.md" $ do
    route (setExtension ".html")
    defaultCompile (defaultContext <> constField "title" "Home")

defaultCompile :: Context String -> Rules ()
defaultCompile ctx =
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/default.html" ctx
    >>= relativizeUrls
    >>= cleanIndexUrls

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
