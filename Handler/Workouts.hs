{-# LANGUAGE OverloadedStrings #-}

module Handler.Workouts where

import Import
import Yesod.Auth
import qualified Database.Esqueleto as E

getWorkoutsR :: Handler Html
getWorkoutsR = do
   defaultLayout $ do
       workouts

workouts :: Widget
workouts = do
    both <- handlerToWidget $ currentUserWorkouts
    [whamlet|
        <p> Your past exercises:
        $forall (e,et) <- entityVal2 both
            <div.well>
                #{exerciseTypeName et} #{exerciseReps e} x #{unKilo (exerciseWeight e)}
    |]


currentUserWorkouts = requireAuthId >>= exercisesForUserId

exercisesForUserId uid = runDB $ 
    E.select $ E.from $ \(e,et) -> do
    E.where_ ( (e E.^. ExerciseUserId E.==. E.val uid) E.&&. (e E.^. ExerciseKindId E.==. et E.^. ExerciseTypeId) )
    return (e,et)


postWorkoutsR :: Handler Html
postWorkoutsR = error "undefined yet"
