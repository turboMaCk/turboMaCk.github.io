--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import Data.Monoid (mappend)
import Control.Monad (when)

import Hakyll
import Hakyll.Contrib.Elm
import Hakyll.Web.Sass (sassCompiler)

--------------------------------------------------------------------------------

postCtx :: Tags -> Context String
postCtx tags =
    mconcat
        [ dateField "date" "%B %e, %Y"
        , tagsField "tags" tags
        , defaultContext
        ]

compressCssItem :: Item String -> Item String
compressCssItem =
    fmap compressCss

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

        tags <- buildTags "posts/*" (fromCapture "tags/*.html")

        match postsPattern $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
                >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
                >>= relativizeUrls

        match "favicon.png" $ do
            route idRoute
            compile copyFileCompiler

        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll postsPattern
                let archiveCtx =
                        listField "posts" (postCtx tags) (return posts) `mappend`
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
                        listField "posts" (postCtx tags) (return posts) `mappend`
                        defaultContext

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
