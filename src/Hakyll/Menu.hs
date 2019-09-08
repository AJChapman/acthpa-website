{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Hakyll.Menu
  ( addToMenu
  , getMenu
  ) where

-- Borrowed from https://github.com/athas/sigkill.dk/blob/master/sigkill.lhs

import Control.Monad                   (zipWithM_)
import Data.List                       (delete, insert, partition)
import Data.Maybe                      (fromMaybe)
import Hakyll
import System.FilePath                 (dropExtension, dropFileName,
                                        dropTrailingPathSeparator,
                                        hasTrailingPathSeparator, normalise,
                                        splitPath, takeBaseName, takeFileName)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5                ((!))

import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

-- Hierarchical menu
---

-- We are going to define a data type and associated helper functions for
-- generating a menu.  Conceptually, the site is a directory tree, with a
-- page being a leaf of the tree.  The menu for a given page will
-- illustrate the path taken from the root to the page, namely which
-- intermediary directories were entered.
--
-- A level (or "line", if you look at its actual visual appearance) of
-- the menu consists of two lists: the elements preceding and succeeding
-- the *focused element*.  The focused element itself is the first
-- element of the `aftItems` list.  This definition ensures that we have
-- at most a single focused element per menu level.  Each element is a
-- pair consisting of a URL and a name.

data MenuItem = MenuItem
  { itemPath :: FilePath
  , itemName :: String
  } deriving (Eq, Ord, Show)

data MenuLevel = MenuLevel
  { prevItems :: [MenuItem]
  , aftItems  :: [MenuItem]
  } deriving (Eq, Ord, Show)

allItems :: MenuLevel -> [MenuItem]
allItems l = prevItems l ++ aftItems l

emptyMenuLevel :: MenuLevel
emptyMenuLevel = MenuLevel [] []

-- First, let us define a function for inserting an element into a sorted
-- list, returning the original list if the element is already there.
insertUniq :: Ord a => a -> [a] -> [a]
insertUniq x xs | x `elem` xs = xs
                | otherwise = insert x xs

-- We can use this function to insert a non-focused element into a
-- `MenuLevel`.  We take care to put the new element in its proper sorted
-- position relative to the focused element, if any.
insertItem :: MenuLevel -> MenuItem -> MenuLevel
insertItem l v = case aftItems l of
                   []     -> atPrev
                   (x:xs) | v < x     -> atPrev
                          | otherwise -> l { aftItems = x:insertUniq v xs }
  where atPrev = l { prevItems = insertUniq v (prevItems l) }

-- When inserting a focused element, we have to split the elements into
-- those that go before and those that come after the focused element.
insertFocused :: MenuLevel -> MenuItem -> MenuLevel
insertFocused l v = MenuLevel bef (v:aft)
  where (bef, aft) = partition (<v) (delete v $ allItems l)

-- Finally, a menu is just a list of menu levels.
newtype Menu = Menu { menuLevels :: [MenuLevel] }

emptyMenu :: Menu
emptyMenu = Menu []

-- I am using the [BlazeHTML](http://jaspervdj.be/blaze/) library for
-- HTML generation, so the result of rendering a menu is an `H.Html`
-- value.  The rendering will consist of one HTML `<ul>` block per menu
-- level, each with the CSS class `menuN`, where `N` is the number of the
-- level.
showMenu :: Menu -> H.Html
showMenu = zipWithM_ showMenuLevel [0..] . menuLevels

-- The focus element is tagged with the CSS class `thisPage`.
showMenuLevel :: Int -> MenuLevel -> H.Html
showMenuLevel d m =
  H.ul (mapM_ H.li elems) ! A.class_ (H.toValue $ "menu" ++ show d)
  where showElem MenuItem{..} = H.a (H.toHtml itemName) ! A.href (H.toValue itemPath)
        showFocusElem item = showElem item ! A.class_ "active"
        elems = map showElem (prevItems m) ++
                case aftItems m of []     -> []
                                   (l:ls) -> showFocusElem l :
                                             map showElem ls

-- Building the menu
---

-- Recall that the directory structure of the site is a tree.  To
-- construct a menu, we are given the current node (page) and a list of
-- all possible nodes of the tree (all pages on the site), and we then
-- construct the minimum tree that contains all nodes on the path from
-- the root to the current node, as well as all siblings of those nodes.
-- In file system terms, we show the files contained in each directory
-- traversed from the root to the current page (as well as any children
-- of the current page, if it is a directory).
--
-- To begin, we define a function that given the current path, decomposes
-- some other path into the part that should be visible.  For example:
relevant :: FilePath -> FilePath -> [FilePath]
relevant this other = relevant' (splitPath this) (splitPath other)
  where relevant' (x:xs) (y:ys) = y : if x == y then relevant' xs ys else []
        relevant' [] (y:_)      = [y]
        relevant' _ _           = []

-- To construct a full menu given the current path and a list of all
-- paths, we repeatedly extend it by a single path.  Recall that menu
-- elements are pairs of names and paths - we generate those names by
-- taking the file name and dropping the extension of the path, also
-- dropping any trailing "index.html" from paths.
buildMenu :: FilePath -> [FilePath] -> Menu
buildMenu this =
  foldl (extendMenu this) emptyMenu . map buildItem
  where
    buildItem :: FilePath -> MenuItem
    buildItem path =
      MenuItem (dropIndex path) (dropExtension . takeFileName $ path)

dropIndex :: FilePath -> FilePath
dropIndex p | takeBaseName p == "index" = dropFileName p
            | otherwise                 = p

extendMenu :: FilePath -> Menu -> MenuItem -> Menu
extendMenu this m MenuItem{..} =
  if path' `elem` ["./", "/", ""] then m else
    Menu $ add (menuLevels m) (relevant this' path') "/"
    where
      add :: [MenuLevel] -> [FilePath] -> [Char] -> [MenuLevel]
      add ls [] _ = ls
      add ls (x:xs) p
        | x `elem` focused = insertFocused l (MenuItem (p ++ x) name') : add ls' xs (p++x)
        | otherwise        = insertItem l (MenuItem (p++x) name') : add ls' xs (p++x)
        where (l,ls') = case ls of []   -> (emptyMenuLevel, [])
                                   k:ks -> (k,ks)
              name' = if hasTrailingPathSeparator x
                        then dropTrailingPathSeparator x
                        else itemName
      focused = splitPath this'
      path' = normalise itemPath
      this' = normalise this

-- For convenience, we define a Hakyll rule that adds anything currently
-- matched to the menu.  To do this, we first need two convenience
-- functions.  The first checks whether the identifier of the current
-- compilation is defined with some other version (recall that a version
-- is identified by a `Maybe String`, not just a `String`), and if so,
-- returns the route of that identifier.
routeWithVersion :: Maybe String -> Compiler (Maybe FilePath)
routeWithVersion v = getRoute =<< setVersion v <$> getUnderlying

-- The second extracts the route for the current identifier with no
-- version.  As a matter of convenience, we return an empty path if the
-- identifier has no associated route.  This should never occur in
-- practice.
normalRoute :: Compiler FilePath
normalRoute = fromMaybe "" <$> routeWithVersion Nothing

-- The `"menu"` version will contain an identifier for every page that
-- should show up in the site menu, with the compiler for each identifier
-- generating a pathname.
addToMenu :: Rules ()
addToMenu = version "menu" $ compile $ makeItem =<< normalRoute

-- To generate the menu for a given page, we use `loadAll` to obtain a
-- list of everything with the version "menu" (the pathnames) and use it
-- to build the menu, which is immediately rendered to HTML.  If a
-- compiler has been defined for these identifiers that creates anything
-- but a `FilePath`, Hakyll will signal a run-time type error.
getMenu :: Compiler String
getMenu = do
  menu <- getMenuItemBodies
  myRoute <- getRoute =<< getUnderlying
  return $ renderHtml $ showMenu $ case myRoute of
    Nothing -> buildMenu "" menu
    Just me -> buildMenu me menu

getMenuItems :: Compiler [Item String]
getMenuItems = loadAll (fromVersion $ Just "menu")

getMenuItemBodies :: Compiler [String]
getMenuItemBodies = map itemBody <$> getMenuItems
