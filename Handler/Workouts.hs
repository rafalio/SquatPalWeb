module Handler.Workouts where

import Import
import Yesod.Auth
import Data.Time
import Control.Arrow

getWorkoutsR :: Handler Html
getWorkoutsR = do
   userId <- requireAuthId 
   exercises <- runDB $ selectList [ExerciseUserId ==. userId] []
   (logNewExerciseFormWidget, enctype) <- generateFormPost (logExerciseForm userId)
   defaultLayout $ do
       [whamlet|
        <p> Your exercises:
        $forall Entity _ exercise <- exercises
            #{show exercise}
       |]
       [whamlet|
          <form method=post action=@{WorkoutsR}>
              ^{logNewExerciseFormWidget}
              <button>Log!
       |]


logExerciseForm :: UserId -> Form Exercise
logExerciseForm uid = renderDivs $ Exercise <$>
    pure uid <*>
    areq (selectField exerciseTypes) "Exercise Type" Nothing <*>
    areq intField "Weight" Nothing <*>
    areq intField "Reps"   Nothing <*>
    aopt textareaField "Notes" Nothing <*>
    lift (liftIO getCurrentTime)
    where
        exerciseTypes = do
            exercisesForUser <- runDB $ selectList [ExerciseTypeCreatedBy ==. uid] []
            optionsPairs $ map ((exerciseTypeName . entityVal) &&& entityKey) exercisesForUser

postWorkoutsR :: Handler Html
postWorkoutsR = do
    uid <- requireAuthId
    ((result, widget),enctype) <- runFormPost (logExerciseForm uid)
    case result of
        FormSuccess newExercise -> do
            runDB (insert newExercise)
            setMessage "Succesfuly added your exercise!"
        _ -> setMessage "There was a problem making your exercise"
    redirect WorkoutsR 
