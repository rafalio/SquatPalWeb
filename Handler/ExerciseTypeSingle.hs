module Handler.ExerciseTypeSingle where

import Import

getExerciseTypeSingleR :: ExerciseTypeId -> Handler Html
getExerciseTypeSingleR = error "Not yet implemented: getExerciseTypeSingleR"

deleteExerciseTypeSingleR :: ExerciseTypeId -> Handler Html
deleteExerciseTypeSingleR eid = do
    runDB $ delete eid
    setMessage "Succesfuly deleted"
    redirect ExerciseTypeR
