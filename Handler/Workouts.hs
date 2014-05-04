{-# LANGUAGE OverloadedStrings #-}

module Handler.Workouts where

import Import
import Yesod.Auth
import qualified Database.Esqueleto as E
import Text.Printf
import Data.Time.Format
import Data.Time.Clock
import System.Locale

getWorkoutsR :: Handler Html
getWorkoutsR = do
   defaultLayout $ do
       setTitle "SquatPal: Workouts"
       workouts

workouts :: Widget
workouts = do
    both <- handlerToWidget $ currentUserWorkouts
    wPref <- handlerToWidget $ userWeightPref <$> requireUser
    toWidget $ [lucius|
        div.workoutEntry{
            padding-top: 0px;
            padding-bottom: 0px;

            h3{
                margin-top: -5px;
            }
        }
    |]
    [whamlet|
        <h2> All Workouts
        $forall (e,et) <- entityVal2 both
            <div.row>
                <div.col-xs-6>
                    <div.well.well-sm.workoutEntry>
                        <h6> #{prettyShowTime (exerciseStarted e)}
                        <h3> #{exerciseTypeName et} #{exerciseReps e} x #{prettyShowWeight (weightForPref wPref (exerciseWeight e))} #{show wPref}
    |]

prettyShowTime :: UTCTime -> String
prettyShowTime = formatTime defaultTimeLocale "%F"

prettyShowWeight :: Double -> String
prettyShowWeight d
    | abs (d - fromIntegral (round d) ) > 0.1 = printf "%.1f" d
    | otherwise = printf "%.0f" d

weightForPref :: WeightPref -> Weight -> Double
weightForPref Kg  = unKilo
weightForPref Lbs = kg2lbs . unKilo


currentUserWorkouts = requireAuthId >>= exercisesForUserId

exercisesForUserId uid = runDB $ 
    E.select $ E.from $ \(e,et) -> do
    E.where_ ( (e E.^. ExerciseUserId E.==. E.val uid) E.&&. (e E.^. ExerciseKindId E.==. et E.^. ExerciseTypeId) )
    E.orderBy [E.desc (e E.^. ExerciseStarted)]
    return (e,et)


postWorkoutsR :: Handler Html
postWorkoutsR = error "undefined yet"
