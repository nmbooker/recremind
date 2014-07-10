{-# LANGUAGE OverloadedStrings #-}

module Recremind.Templates (
    appTemplate
,   setRecForm
) where

import           Control.Monad (when)
import           Text.Blaze ((!))
import           Text.Blaze.Html (toHtml)
import           Text.Blaze.Internal (preEscapedText)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.String as S
import Data.Monoid (mempty)

setRecForm postAction =
    appTemplate "Set Record Reminder" [] $ do
        H.div ! A.class_ "container" $ do
            H.h1 "Set Record Reminder"
            H.form  ! A.enctype "multipart/form-data"
                    ! A.method "POST"
                    ! A.action "/setrec" $ do
                      H.div ! A.class_ "form-group" $ do
                          H.label ! A.for "in_progname" $ "Program Name:"
                          H.input   ! A.type_ "text"
                                            ! A.name "progname"
                                            ! A.size "10"
                                            ! A.id "in_progname"
                                            ! A.class_ "form-control"
                      H.div ! A.class_ "form-group" $ do
                          H.label "When:"
                          H.input ! A.type_ "datetime-local"
                                          ! A.name  "when"
                                          ! A.class_ "form-control"
                      H.div ! A.class_ "form-group" $ do
                          H.input ! A.type_ "submit"
                                  ! A.name "setReminder"
                                  ! A.class_ "btn btn-default"

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

        mapM preEscapedText [
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
