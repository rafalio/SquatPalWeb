{-# LANGUAGE OverloadedStrings #-}
module Handler.Log where

import Import
import Yesod.Auth
import Control.Arrow
import Data.Time

getLogR :: Handler Html
getLogR = do
    userId <- requireAuthId 
    {-(logNewExerciseFormWidget, enctype) <- generateFormPost (logExerciseForm userId)-}
    (logNewExerciseFormWidget, enctype) <- generateFormPost logExerciseFormM
    defaultLayout $ do
        toWidget [lucius|
            form#logExercise div{
                padding: 5px; 
            }
        |]
        [whamlet|
        <h3> Log your exercise
        <form.form-inline #logExercise method=post action=@{LogR}>
            <div.row>
                ^{logNewExerciseFormWidget}
                 <div.form-group>
                     <button.btn.btn-success>Log it!
        |]


postLogR :: Handler Html
postLogR = do 
    uid <- requireAuthId
    ((result, widget),enctype) <- runFormPost (logExerciseForm uid)
    case result of
        FormSuccess newExercise -> do
            runDB (insert newExercise)
            setMessageT MsgSuccess "Succesfuly added your exercise!"
        _ -> setMessageT MsgError "There was a problem making your exercise"
    redirect LogR



hConfig = BootstrapHorizontalForm (ColMd 2) (ColMd 4) (ColXs 2) (ColXs 3)

logExerciseForm :: UserId -> Form Exercise
logExerciseForm uid = renderBootstrap3 hConfig $ Exercise <$>
        pure uid <*>
        areq (selectField exerciseTypes) (bfs ("Exercise Type"::Text)) Nothing <*>
        areq intField (bfs ("Weight"::Text)) Nothing <*>
        areq intField (bfs ("Reps"::Text)) Nothing <*>
        pure Nothing <*>
        lift (liftIO getCurrentTime) <*
        (bootstrapSubmit ("add" :: BootstrapSubmit Text))
        where
            exerciseTypes = do
                exercisesForUser <- runDB $ selectList [ExerciseTypeCreatedBy ==. uid] []
                optionsPairs $ map ((exerciseTypeName . entityVal) &&& entityKey) exercisesForUser



logExerciseFormM :: Form Exercise
logExerciseFormM extra = do
    uid  <- lift requireAuthId
    user <- lift requireUser

    (typeRes, typeView)      <- mreq (selectField $ exerciseTypes uid) (bfs ("Exercise Type" :: Text)) Nothing
    (weightRes, weightView)  <- mreq intField (bfs ("Weight"::Text)) Nothing
    (repsRes, repsView)      <- mreq intField (bfs ("Reps"::Text)) Nothing
    time <- lift (liftIO getCurrentTime)
    let weightPref = show . userWeightPref $ user

    let logRes = Exercise <$> 
            pure uid <*> 
            typeRes <*> 
            weightRes <*> 
            repsRes <*> 
            pure Nothing <*> 
            pure time

    let widget = do
        toWidget [lucius|
            ##{fvId typeView}{
                width: 200px;
            }
           #weightDiv{
                width:150px;
            }
           #repsDiv{
               width:150px;
           }
       |]
        [whamlet|
#{extra}
<div.form-group>
    ^{fvInput typeView}
<div.form-group #weightDiv>
    <div.input-group>
        ^{fvInput weightView}
        <span.input-group-addon>#{weightPref}
<span>
    X
<div.form-group #repsDiv>
    <div.input-group>
        ^{fvInput repsView}
        <span.input-group-addon>reps
        |]
    return (logRes, widget)
    where
        exerciseTypes uid = do
            exercisesForUser <- runDB $ selectList [ExerciseTypeCreatedBy ==. uid] []
            optionsPairs $ map ((exerciseTypeName . entityVal) &&& entityKey) exercisesForUser





