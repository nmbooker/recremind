{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module BlazeDateTime (
    inputDate
) where


import           Data.Text (Text)
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Digestive

-- Old but could be used as a basis for a digestive-functors form using proper datetime widget in HTML5
inputDate :: Text -> View v -> H.Html
inputDate ref view =
    H.input
        !   A.type_ "datetime-local"
        !   A.id    (H.toValue ref')
        !   A.name  (H.toValue ref')
        !   A.value (H.toValue $ fieldInputText ref view)
    where
        ref' = absoluteRef ref view
