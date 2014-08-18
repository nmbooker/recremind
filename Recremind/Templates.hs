{-# LANGUAGE OverloadedStrings #-}

module Recremind.Templates (
    appTemplate
,   setRecFormSpec
,   setRecView
) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (when)
import           Data.Text (Text)
import           Text.Blaze ((!))
import           Text.Blaze.Internal (preEscapedText, MarkupM)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid (mempty)

import Text.Digestive
import Text.Digestive.Blaze.Html5


import Recremind.Scheduler

setRecFormSpec :: Monad m => Form Text m Reminder
setRecFormSpec = Reminder
    <$> "progname" .: string Nothing
    <*> "channel" .: string Nothing
    <*> "when" .: localTimeFormlet "%d/%m/%Y" "%H:%M" Nothing
    <*> "recordLimit" .: stringRead "Can't parse number" (Just 7)

setRecView :: View H.Html -> H.Html
setRecView view = do
        H.div ! A.class_ "container" $ do
            H.h1 "Set Record Reminder"
            childErrorList "" view

            divFormGroup $ do
                label "progname" view "Program Name:"
                formControl $ inputText "progname" view

            divFormGroup $ do
                label "channel" view "Channel:"
                formControl $ inputText "channel" view

            divFormGroup $ do
                label "when" view "When:"
                formControl $ inputText "when" view

            divFormGroup $ do
                label "recordLimit" view "Recording Limit (days):"
                formControl $ inputText "recordLimit" view

            divFormGroup $ do
                formControl $ inputSubmit "Signup"

-- divFormGroup -- candidate to go into a Bootstrap library
divFormGroup :: H.Html -> H.Html
divFormGroup h =
    H.div ! A.class_ "form-group" $ h

-- formControl -- candidate to go into a Bootstrap library
formControl :: H.Html -> H.Html
formControl h = (h ! A.class_ "form-control")

inputDate :: Text -> View v -> H.Html
inputDate ref view =
    H.input
        !   A.type_ "datetime-local"
        !   A.id    (H.toValue ref')
        !   A.name  (H.toValue ref')
        !   A.value (H.toValue $ fieldInputText ref view)
    where
        ref' = absoluteRef ref view


appTemplate :: String -> [H.Html] -> H.Html -> H.Html
appTemplate title headers body =
    H.docTypeHtml $
        H.html $ do
          H.head $ do
            H.title (H.toHtml title)
            H.meta ! A.httpEquiv "Content-Type"
                   ! A.content "text/html;charset=utf-8"
            twitterBootstrap True
            sequence_ headers
          H.body $ do
            navBar >> body
            H.script ! A.src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js" $ mempty
            H.script ! A.src "//netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js" $ mempty

twitterBootstrap :: Bool -> MarkupM ()
twitterBootstrap includeTheme = do
        H.meta  ! A.httpEquiv   "X-UA-Compatible"
                ! A.content     "IE=edge"
        H.meta  ! A.name    "viewport"
                ! A.content "width=device-width, initial-scale=1"
        H.link  ! A.rel "stylesheet"
                ! A.href "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css" ! A.media "screen"
        when includeTheme $ H.link
            ! A.rel "stylesheet"
            ! A.href "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap-theme.min.css"

        mapM_ preEscapedText [
              "<!--[if lt IE 9]>"
            , "    <script src=\"https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js\"></script>"
            , "    <script src=\"https://oss.maxcdn.com/respond/1.4.2/respond.min.js\"></script>"
            , "<![endif]-->"
            ]

        H.script (return ())
            ! A.src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"
        H.script (return ()) ! A.src "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"


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

