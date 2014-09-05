{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts, KindSignatures #-}

module Main where

import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf
import Recremind.Templates (appTemplate, setRecFormSpec, setRecView)
import Recremind.Scheduler (Reminder, whenToRemind, reminderSubj, reminderBody)
import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Happstack.Server
import qualified Text.Blaze.Html5 as H

import Text.Digestive.Blaze.Html5
import Text.Digestive.Happstack

import Atd.Calc (atTime, messageScript, Script, Recipient)
import Atd.IO (scheduleAt)

import System.Locale (defaultTimeLocale)

recordReminderApp :: Recipient -> ServerPart Response
recordReminderApp email = msum
        [   dir "setrec" $ setRecHandler email
        --,   seeOther "/setrec" "/setrec"
        ]

setRecHandler :: Recipient -> ServerPart Response
setRecHandler email = do
    decodeBody $ defaultBodyPolicy "/tmp/" 0 40960 40960
    r <- runForm "test" setRecFormSpec
    case r of
        (view, Nothing) -> do
            let view' = fmap H.toHtml view
            reply "Set record reminder" [] $
                form view' "/setrec" (setRecView view')

        (_, Just response) -> do
            let (atArgs, atScript) = reminderToAt email response
            let atTech = do
                H.h2 "Arguments for at:"
                H.p $ H.pre $ H.preEscapedToHtml $ show atArgs
                H.h2 "Script for at:"
                H.p $ H.pre $ H.preEscapedToHtml $ atScript
            atResponse <- liftIO $ scheduleAt atArgs atScript
            case atResponse of
                Left (errno, output) -> reply "At failed" [] $ do
                    H.h2 "ERROR at scheduling failed"
                    H.p $ H.toHtml $ "Exit status: " ++ (show errno)
                    H.pre $ H.preEscapedToHtml output
                    atTech
                Right output -> reply "At succeeded" [] $ do
                    H.h2 "at succeeded"
                    H.pre $ H.preEscapedToHtml output
                


reminderToAt :: Recipient -> Reminder -> ([String], Script)
reminderToAt email reminder = (atArgs, atScript)
    where   atArgs = atTime defaultTimeLocale $ whenToRemind reminder
            atScript = messageScript email subj message
            subj = reminderSubj reminder
            message = reminderBody reminder

reply :: forall (m :: * -> *).
         FilterMonad Response m =>
         String         -- ^ Page title for <title> tag
         -> [H.Html]    -- ^ List of additional bits for <head> section
         -> H.Html      -- ^ Body content
         -> m Response  -- ^ OK response for Happstack

reply title headers theBody = ok $ toResponse $
    appTemplate title headers theBody


main :: IO ()
main = do
    maybeEmail <- lookupEnv "RECREMIND_TO_EMAIL"
    case maybeEmail of
        Nothing -> do
            hPutStrLn stderr "FATAL: RECREMIND_TO_EMAIL not set"
            exitFailure
        Just email -> do
            conf <- serverConf
            putStrLn $ printf "Starting server on port %d..." (port conf)
            simpleHTTP conf $ recordReminderApp email

-- IO so can get some bits from environment later :)
serverConf :: IO (Conf)
serverConf = do
    maybePort <- lookupEnv "PORT"
    let thePort = case maybePort of
            Nothing -> 8000
            Just p -> (read p) :: Int
    return $ nullConf { port = thePort }
