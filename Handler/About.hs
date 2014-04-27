module Handler.About where

import Import

getAboutR :: Handler Html
getAboutR = defaultLayout $ do
  $(widgetFile "about")
