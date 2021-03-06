{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Recremind.Templates (
    appTemplate
,   setRecFormSpec
,   setRecView
) where

import           Control.Applicative ((<$>), (<*>))
import           Data.Text (Text)
import           Text.Blaze ((!))
import           Text.Blaze.Internal (MarkupM)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid (mempty)

import Text.Digestive
import Text.Digestive.Blaze.Html5

import Bootstrap (bootstrapTemplate, divFormGroup, formControl)
import Recremind.Scheduler

setRecFormSpec :: Monad m => Form Text m Reminder
setRecFormSpec = Reminder
    <$> "progname" .: string Nothing
    <*> "channel" .: string Nothing
    <*> "when" .: localTimeFormlet "%d/%m/%Y" "%H:%M" Nothing
    <*> "recordLimit" .: stringRead "Can't parse number" (Just 7)

setRecView :: View H.Html -> H.Html
setRecView view =
        H.div ! A.class_ "container" $ do
            H.h1 "Set Record Reminder"
            childErrorList "" view

            divFormGroup $ do
                label "progname" view "Program Name:"
                formControl $ inputText "progname" view

            divFormGroup $ do
                label "channel" view "Channel:"
                formControl $ inputText "channel" view

            dateTimeView $ subView "when" view

            divFormGroup $ do
                label "recordLimit" view "Recording Limit (days):"
                formControl $ inputText "recordLimit" view

            divFormGroup $
                formControl $ inputSubmit "Signup"

dateTimeView :: forall v. View v -> Text.Blaze.Internal.MarkupM ()
dateTimeView view = do
    divFormGroup $ do
        label "date" view "When (Date):"
        formControl $ inputText "date" view

    divFormGroup $ do
        label "time" view "When (Time):"
        formControl $ inputText "time" view


appTemplate :: String -> [H.Html] -> MarkupM () -> H.Html
appTemplate title headers body =
    bootstrapTemplate title headers $ do
        navBar
        body

navBar :: H.Html
navBar =
    -- H.div ! A.class_ "navbar navbar-default navbar-static-top" $ H.div ! A.class_ "container" $ do
           mempty
           --H.div ! A.class_ "navbar-header" $ do
           --  H.button ! A.type_ "button"
           --         ! A.class_ "navbar-toggle" ! H.dataAttribute "toggle" "collapse" ! H.dataAttribute "target" ".navbar-collapse" $ do
           --    H.a ! A.class_ "navbar-brand" ! A.href "#" $ "λ"
           --H.div ! A.class_ "navbar-collapse collapse" $ H.ul ! A.class_ "nav navbar-nav" $ do
           --  H.li ! A.class_ "active" $ H.a ! A.href "#" $ "Home"
           --  H.li $ H.a ! A.href "#about" $ "About"
           --  H.li $ H.a ! A.href "#contact" $ "Contact"

