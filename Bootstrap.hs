{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Bootstrap (
    bootstrapTemplate
,   divFormGroup
,   formControl
) where

import           Control.Monad (when)
import           Text.Blaze ((!))
import           Text.Blaze.Internal (preEscapedText, MarkupM)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid (mempty)

bootstrapTemplate :: String -> [H.Html] -> H.Html -> H.Html
bootstrapTemplate title headers body =
    H.docTypeHtml $
        H.html $ do
          H.head $ do
            H.title (H.toHtml title)
            H.meta ! A.httpEquiv "Content-Type"
                   ! A.content "text/html;charset=utf-8"
            twitterBootstrap True
            sequence_ headers
          H.body $ do
            body
            H.script ! A.src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js" $ mempty
            H.script ! A.src "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js" $ mempty

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


divFormGroup :: H.Html -> H.Html
divFormGroup h =
    H.div ! A.class_ "form-group" $ h


formControl :: H.Html -> H.Html
formControl h = (h ! A.class_ "form-control")
