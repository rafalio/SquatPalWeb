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
        setTitle "SquatPal | Settings"
        [whamlet|
    <h2> Settings
    <h4> Your exercises
    $forall (Entity id (ExerciseType name _ _)) <- yourExercises
        <div.row>
            <div.col-xs-9.col-sm-5>
                <div.well.well-sm> #{name}
            <div.col-xs-3.col-sm-2>
                <form style="display:inline" method=post action=@{ExerciseTypeSingleR id}?_method=DELETE>
                    <button.btn.btn-danger>Delete
    <form method=post action=@{ExerciseTypeR} enctype=#{enctype1}>
        <div.row>
            <div.col-xs-9.col-sm-5>
                ^{widget}
            <div.col-xs-3.col-sm-2>
                <button.btn.btn-success>Add!
    <br />
    <form.form-inline method=post action=@{SettingsR}>
        <div.row>
            <div.col-xs-5.col-md-3>
                Weight preference ^{form}
            <div.col-xs-3.col-md-2>
                <button.btn.btn-default type=submit>Update
 |]

exerciseTypeForm :: Form ExerciseType
exerciseTypeForm = renderBootstrap3 BootstrapInlineForm $ ExerciseType <$> 
    areq textField (bfs ("Exercise Name"::Text)) Nothing <*>
    lift requireAuthId <*>
    pure WithWeight


weightPrefForm :: Maybe User -> Form WeightPref
weightPrefForm mu = renderBootstrap3 BootstrapInlineForm $ 
    areq 
        (selectFieldList enumTypeTuples)
        (bfs ("Weight unit"::Text))
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
    ((result, _),_) <- runFormPost exerciseTypeForm 
    case result of
        FormSuccess extype -> do
            runDB (insertUnique extype) >>= maybe 
                                            (setMessageT MsgWarning "You already have an exercise with this name.") 
                                            (const $ setMessageT MsgSuccess "Exercise succesfuly created")
        _ -> setMessageT MsgError "There was a problem making your exercise"
    redirect SettingsR
 
