module Handler.Log where

import Import
import Yesod.Auth
import Control.Arrow
import Data.Time

getLogR :: Handler Html
getLogR = do
    userId <- requireAuthId 
    (logNewExerciseFormWidget, enctype) <- generateFormPost (logExerciseForm userId)
    defaultLayout $ do
        [whamlet|
            <div class="col-lg-6">
               <form method=post action=@{LogR} class="form-horizontal">
                   ^{logNewExerciseFormWidget}
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
        areq intField ("Weight") Nothing <*>
        areq intField "Reps" Nothing <*>
        aopt textareaField "Notes" Nothing <*>
        lift (liftIO getCurrentTime) <*
        (bootstrapSubmit ("add" :: BootstrapSubmit Text))
        where
            exerciseTypes = do
                exercisesForUser <- runDB $ selectList [ExerciseTypeCreatedBy ==. uid] []
                optionsPairs $ map ((exerciseTypeName . entityVal) &&& entityKey) exercisesForUser

