{-

Copyright (c) 2016, Marek Fajkus
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the author nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL MAREK FAJKUS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

My blog builder on top of Hakyll & Haskell

 \    \\     \
  \    \\     \
   \    \\     \ _____________
    \    \\     \\            \
     \    \\     \\____________\
      \    \\     \ _____________
      /    //      \\            \
     /    //        \\____________\
    /    //    /\    \
   /    //    /  \    \
  /    //    /    \    \
 /____//____/      \____\
    _    _           _        _ _
   | |  | |         | |      | | |
   | |__| | __ _ ___| | _____| | |
   |  __  |/ _` / __| |/ / _ \ | |
   | |  | | (_| \__ \   <  __/ | |
   |_|  |_|\__,_|___/_|\_\___|_|_|

-}
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import Data.Monoid ((<>))
import Control.Monad (when)

import Hakyll
import Hakyll.Web.Sass (sassCompiler)

--------------------------------------------------------------------------------

{-
    This function handles context metadata for posts
-}
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

{-
    This function cleans cached preview (unpublished blogposts)
    Preview are thing hacked o top of hakyll which makes it easier
    to work on more changes posts in prallel when I want to.
-}
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
