module Notifications where

import Yesod
import Prelude
import Text.Blaze
import qualified Text.Blaze.Html5 as Blaze
import qualified Text.Blaze.Html5.Attributes as Blaze
import Text.Blaze.Renderer.Utf8
import Data.Monoid
import Data.List


data NotificationMsgType = MsgSuccess | MsgInfo | MsgWarning | MsgError
    deriving (Eq, Read, Show, Enum)

data NotificationMessage = NotificationMessage NotificationMsgType Html

instance ToMarkup NotificationMessage where
    toMarkup (NotificationMessage k t) = do
        let classes = Blaze.class_ $ mconcat ["alert ","alert-dismissable ", toValue k] -- hack, shouldn't be spaces
        Blaze.div ! classes $ do
            (Blaze.button ! Blaze.type_ "button" ! Blaze.class_ "close" ! Blaze.dataAttribute "dismiss" "alert" $ "x")
            t

instance ToValue NotificationMsgType where
    toValue MsgSuccess     = "alert-success"
    toValue MsgInfo        = "alert-info"
    toValue MsgWarning     = "alert-warning"
    toValue MsgError       = "alert-danger"


-- Set a flash message with a type (Error/Success etc)
-- It will render appropriately
setMessageT :: (MonadHandler m) => NotificationMsgType -> Html -> m ()
setMessageT typ text = setMessage $ toMarkup $ NotificationMessage typ text
