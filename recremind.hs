module Main where

import Recremind.Templates (appTemplate, setRecForm)
import Control.Monad (msum)
import Happstack.Server


recordReminderApp :: ServerPart Response
recordReminderApp = msum
        [   dir "setrec" $ setRecGetHandler
        --,   seeOther "/setrec" "/setrec"
        ]

setRecGetHandler :: ServerPart Response
setRecGetHandler =
    do  method GET
        ok $ toResponse $ (setRecForm "/setrec")



main :: IO ()
main = simpleHTTP nullConf $ recordReminderApp
