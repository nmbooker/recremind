{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts, KindSignatures #-}

module Main where

import Text.Printf
import Recremind.Templates (appTemplate, setRecFormSpec, setRecView)
import Control.Monad (msum)
import Happstack.Server
import qualified Text.Blaze.Html5 as H

import Text.Digestive.Blaze.Html5
import Text.Digestive.Happstack

recordReminderApp :: ServerPart Response
recordReminderApp = msum
        [   dir "setrec" $ setRecHandler
        --,   seeOther "/setrec" "/setrec"
        ]

setRecHandler :: ServerPart Response
setRecHandler = do
    decodeBody $ defaultBodyPolicy "/tmp/" 0 40960 40960
    r <- runForm "test" setRecFormSpec
    case r of
        (view, Nothing) -> do
            let view' = fmap H.toHtml view
            reply "Set record reminder" [] $
                form view' "/setrec" (setRecView view')

        (_, Just response) -> do
            reply "Valid" [] $ do
                H.h1 "Form is valid."
                H.p $ H.toHtml $ show response



reply :: forall (m :: * -> *).
         FilterMonad Response m =>
         String -> [H.Html] -> H.Html -> m Response
reply title headers theBody = ok $ toResponse $ appTemplate title headers theBody

main :: IO ()
main = do
    let listenPort = 8000
    putStrLn $ printf "Starting server on port %d..." listenPort
    simpleHTTP nullConf { port = listenPort } $ recordReminderApp
