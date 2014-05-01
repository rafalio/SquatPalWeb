module Handler.Settings where

import Import
import Yesod.Auth

getSettingsR :: Handler Html
getSettingsR = do
    u <- requireUser
    (form,enctype) <- generateFormPost $ weightPrefForm (Just u)
    defaultLayout $ do
        [whamlet|
            <form method=post action=@{SettingsR}>
                ^{form}
                <button>Change
        |]

weightPrefForm :: Maybe User -> Form WeightPref
weightPrefForm mu = renderDivs $ 
    areq 
        (selectFieldList enumTypeTuples)
        "Weight unit" 
        (userWeightPref <$> mu)

postSettingsR :: Handler Html
postSettingsR = do
    u <- requireUser
    ((result,_),_) <- runFormPost (weightPrefForm (Just u))
    uid <- requireAuthId
    case result of
      FormSuccess wPref -> do
          runDB $ update uid [UserWeightPref =. wPref]
          setMessageT MsgSuccess "Succesfully updated"
      _ -> setMessageT MsgError "There was a problem updating your preference"
    redirect SettingsR
