--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import Data.Monoid ((<>))
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

-- Drafts

cleanPreview :: IO ()
cleanPreview = do
    remove "generated/preview"
    where
        remove dir = do
            putStrLn $ "Removing " ++ dir ++ "..."
            removeDirectory dir

-- RSS feed

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]

feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = "turbo_MaCk - " ++ title
    , feedDescription = "Personal website of turbo_MaCk"
    , feedAuthorName  = "Marek Fajkus"
    , feedAuthorEmail = "marek.faj@gmail.com"
    , feedRoot        = "http://turbomack.github.iuo"
    }

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

        -- Assets
        match "assets/*" $ do
            route idRoute
            compile copyFileCompiler

        -- Styles (SASS)
        match "sass/default.scss" $ do
            route $ setExtension "css" `composeRoutes` gsubRoute "sass/" (const "css/")
            compile (compressCssItem <$> sassCompiler)

        -- Elm
        match "elm/*.elm" $ do
            route $ setExtension "js" `composeRoutes` gsubRoute "elm/" (const "js/")
            compile elmMake

        -- Tags
        tags <- buildTags postsPattern (fromCapture "tags/*.html")

        match postsPattern $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
                >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
                >>= relativizeUrls

        -- Favicon
        match "favicon.png" $ do
            route idRoute
            compile copyFileCompiler

        -- Archive
        create ["archive.html"] $ do
            let title = "Archive"

            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll postsPattern
                let archiveCtx =
                        constField "title" title <>
                        field "tags" (\_ -> renderTagList tags) <>
                        listField "posts" (postCtx tags) (return posts) <>
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls
        -- RSS feed
        create ["rss.xml"] $ do
            route idRoute
            compile $ do
                loadAllSnapshots postsPattern "content"
                    >>= recentFirst
                    >>= renderRss (feedConfiguration "All posts") feedCtx

        -- Post tags
        tagsRules tags $ \tag pattern -> do
            let title = "Posts tagged " ++ tag

            -- Copied from posts, need to refactor
            route $ setExtension "html"
            compile $ do
                posts <- recentFirst =<< loadAll pattern
                let ctx = constField "title" title <>
                            listField "posts" (postCtx tags) (return posts) <>
                            defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

        -- Home
        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- fmap (take 3) . recentFirst =<< loadAll postsPattern
                let indexCtx =
                        listField "posts" (postCtx tags) (return posts) <>
                        defaultContext

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        -- Templates
        match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
