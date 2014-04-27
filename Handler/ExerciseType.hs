module Handler.ExerciseType where

import Import
import Yesod.Auth

exerciseTypeForm :: Form ExerciseType
exerciseTypeForm = renderDivs $ ExerciseType <$> 
    areq textField "Exercise Name" Nothing <*>
    lift requireAuthId


postExerciseTypeR :: Handler Html
postExerciseTypeR = do
    ((result, widget),enctype) <- runFormPost exerciseTypeForm 
    userId <- requireAuthId
    case result of
        FormSuccess extype -> do
            runDB (insertUnique extype) >>= maybe 
                                            (setMessage "You already have an exercise with this name.") 
                                            (const $ setMessage "Exercise succesfuly created")
        _ -> setMessage "There was a problem making your exercise"
    redirect ExerciseTypeR
        

getExerciseTypeR :: Handler Html
getExerciseTypeR = do
    userId <- requireAuthId
    (widget, enctype) <- generateFormPost exerciseTypeForm 
    yourExercises <- runDB $ selectList [ExerciseTypeCreatedBy ==. userId] []
    defaultLayout $ do
       [whamlet|
            <form method=post action=@{ExerciseTypeR} enctype=#{enctype}>
                ^{widget}
                <button>Submit
            <p> Your exercises:
            $forall (Entity id (ExerciseType name _)) <- yourExercises
                #{name}
                <form style="display:inline" method=post action=@{ExerciseTypeSingleR id}?_method=DELETE>
                  <button>Delete
                <br />
        |]







getExerciseR :: ExerciseId -> Handler Html
getExerciseR eid = error "Undefined yet"

