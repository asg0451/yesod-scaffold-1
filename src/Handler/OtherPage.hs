module Handler.OtherPage where

import           Import
import           Text.Julius           (RawJS (..))
import           Yesod.Form.Bootstrap3 (BootstrapFormLayout (..),
                                        renderBootstrap3, withSmallInput)

-- This is a handler function for the GET request method on the OtherPageR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getOtherPageR :: Handler Html
getOtherPageR =
    defaultLayout $
    do setTitle "watwatwat!"
       [whamlet| <p> yo what up
                 <p> mofk    |]
