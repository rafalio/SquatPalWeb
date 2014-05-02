module Handler.Settings where

import Import
import Yesod.Auth

getSettingsR :: Handler Html
getSettingsR = do
    u <- requireUser
    (form,enctype1) <- generateFormPost $ weightPrefForm (Just u)

    userId <- requireAuthId
    (widget, enctype2) <- generateFormPost exerciseTypeForm 
    yourExercises <- runDB $ selectList [ExerciseTypeCreatedBy ==. userId] []

    defaultLayout $ do
        [whamlet|
            <form method=post action=@{SettingsR}>
                ^{form}
                <button>Change
        |]
        [whamlet|
             <form method=post action=@{ExerciseTypeR} enctype=#{enctype1}>
                 ^{widget}
                 <button>Submit
             <p> Your exercises:
             $forall (Entity id (ExerciseType name _ _)) <- yourExercises
                 #{name}
                 <form style="display:inline" method=post action=@{ExerciseTypeSingleR id}?_method=DELETE>
                   <button>Delete
                 <br />
         |]

exerciseTypeForm :: Form ExerciseType
exerciseTypeForm = renderDivs $ ExerciseType <$> 
    areq textField "Exercise Name" Nothing <*>
    lift requireAuthId <*>
    pure WithWeight


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


postExerciseTypeR :: Handler Html
postExerciseTypeR = do
    ((result, widget),enctype) <- runFormPost exerciseTypeForm 
    userId <- requireAuthId
    case result of
        FormSuccess extype -> do
            runDB (insertUnique extype) >>= maybe 
                                            (setMessageT MsgWarning "You already have an exercise with this name.") 
                                            (const $ setMessageT MsgSuccess "Exercise succesfuly created")
        _ -> setMessageT MsgError "There was a problem making your exercise"
    redirect SettingsR
 
