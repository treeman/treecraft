{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import System.FilePath
import Hakyll

siteRoot = "http://www.treecraft.se"

config :: Configuration
config = defaultConfiguration
    { deployCommand = "sync" }

main :: IO ()
main = hakyllWith config $ do
    match ("images/**" .||. "favicon.ico") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match ("_world/**.png" .||. "_world/**.js" .||. "_world/**.css") $ do
        route   worldRoute
        compile copyFileCompiler

    match "index.html" $ do
        route  idRoute
        compile copyFileCompiler

    match ("404.markdown" .||. "ip.markdown") $ do
        route   dropIndexRoute
        compile $ do
            pandocCompiler
                >>= loadAndApplyTemplate "templates/site.html" siteCtx

    match "templates/*" $ compile templateCompiler

-- Move to subdirectories to avoid extensions in links.
dropIndexRoute :: Routes
dropIndexRoute = customRoute $
     (++ "/index.html"). dropExtension . toFilePath

worldRoute :: Routes
worldRoute = gsubRoute "_world/" (const "")

siteCtx :: Context String
siteCtx = mconcat
    [ constField "ip" "treecraft.se:25692"
    , defaultContext
    ]

