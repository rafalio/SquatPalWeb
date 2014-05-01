module Handler.ExerciseTypeSingle where

import Import
import Control.Monad (when)

getExerciseTypeSingleR :: ExerciseTypeId -> Handler Html
getExerciseTypeSingleR = error "Not yet implemented: getExerciseTypeSingleR"

deleteExerciseTypeSingleR :: ExerciseTypeId -> Handler Html
deleteExerciseTypeSingleR eid = do
    c <- runDB $ count [ExerciseKindId ==. eid]
    when (c > 0) $ do
       setMessageT MsgError "You cannot delete an exercise type that has associated exercises"
       redirect ExerciseTypeR 
    runDB $ delete eid
    setMessageT MsgSuccess "Succesfuly deleted"
    redirect ExerciseTypeR
