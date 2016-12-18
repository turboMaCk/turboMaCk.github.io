--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import Data.Monoid (mappend)
import Control.Monad (when)

import Hakyll
import Hakyll.Contrib.Elm
import Hakyll.Web.Sass (sassCompiler)

--------------------------------------------------------------------------------

cleanPreview :: IO ()
cleanPreview = do
  remove "generated/preview"
  where
    remove dir = do
      putStrLn $ "Removing " ++ dir ++ "..."
      removeDirectory dir

--------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs

    let previewMode = elem "watch" args

        clean = elem "clean" args

        hakyllConf = if previewMode
                     then defaultConfiguration { destinationDirectory = "generated/preview" }
                     else defaultConfiguration

        postPattern folder =
          (fromGlob $ folder ++ "/*.md") .||. (fromGlob $ folder ++ "/*.org")

        postsPattern = if previewMode
                       then postPattern "posts" .||. postPattern "posts/drafts"
                       else postPattern "posts"

    putStrLn $ show postsPattern
    putStrLn $ show $ "posts/*.md" .||. "posts/drafts/*.md"

    when clean cleanPreview

    hakyllWith hakyllConf $ do
        match "assets/*" $ do
            route idRoute
            compile copyFileCompiler

        match "sass/*" $ do
            route $ setExtension "css" `composeRoutes` gsubRoute "sass/" (const "css/")
            compile (compressCssItem <$> sassCompiler)

        match "elm/*.elm" $ do
            route $ setExtension "js" `composeRoutes` gsubRoute "elm/" (const "js/")
            compile elmMake

        match postsPattern $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

        match "favicon.png" $ do
            route idRoute
            compile copyFileCompiler

        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll postsPattern
                let archiveCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls

        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll postsPattern
                let indexCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        defaultContext

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

compressCssItem :: Item String -> Item String
compressCssItem = fmap compressCss
