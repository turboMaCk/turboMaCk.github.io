--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)

import Hakyll
import Hakyll.Contrib.Elm
import Hakyll.Web.Sass (sassCompiler)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    match "assets/*" $ do
        route idRoute
        compile copyFileCompiler

    match "sass/*" $ do
        route $ setExtension "css" `composeRoutes` gsubRoute "sass/" (const "css/")
        compile (compressCssItem <$> sassCompiler)

    match "elm/*.elm" $ do
        route $ setExtension "js" `composeRoutes` gsubRoute "elm/" (const "js/")
        compile elmMake

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
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
            posts <- recentFirst =<< loadAll "posts/*"
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
