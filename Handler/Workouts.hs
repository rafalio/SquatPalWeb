{-# LANGUAGE OverloadedStrings #-}

module Handler.Workouts where

import Import
import Yesod.Auth
import qualified Database.Esqueleto as E
import Text.Printf
import Data.Time.Format
import Data.Time.Clock
import System.Locale
import Data.Time.Calendar
import Data.Function (on)
import qualified Data.List as L
import Data.Maybe
import Text.Blaze
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes hiding (map)
import Text.Lucius (CssUrl, luciusFile, luciusFileDebug, renderCss)
import Control.Monad

data ExerciseSet = ExerciseSet {
    setDate :: UTCTime,
    setType :: ExerciseType,
    setExercises :: [Exercise]
} deriving (Show)

data Workout = Workout {
    workoutDate :: UTCTime,
    workoutExercises :: [ExerciseSet]
} deriving (Show)

getWorkoutsR :: Handler TypedContent
getWorkoutsR = do
    email <- userIdent <$> requireUser
    selectRep $ do
       provideRep $ 
         defaultLayout $ do
             setTitle "SquatPal: Workouts"
             workouts
       provideRep $ do
         return $ Import.object $ ["name" .= (email :: Text)]



exerciseToBox :: Exercise -> WeightPref -> Html
exerciseToBox e wPref = html $ do
  H.div ! class_ "well exerciseBox well-sm" $ do
    H.p $ H.toHtml . exerciseReps $ e
    H.hr
    H.p $ H.toHtml $ (prettyShowWeight (weightForPref wPref (exerciseWeight e))) ++ " " ++ (show wPref)


exerciseRow :: [Exercise] -> WeightPref -> Html
exerciseRow es w = html $ do
    H.div ! class_ "row" $ sequence_ $ L.intersperse (H.span ">>") ( map (flip exerciseToBox $ w) es)


workouts :: Widget
workouts = do
    workouts <- handlerToWidget $ currentUserWorkouts
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
    toWidget ($(luciusFile "templates/workouts.lucius"))
    [whamlet|
  <h2> All Workouts
  $forall w <- workouts
      <div.row>
          <div.col-sm-10>
              <h4> #{prettyShowTime (workoutDate w)}
              $forall s <- workoutExercises w
                  <div.pushRight>
                      <h3> #{exerciseTypeName (setType s)}
                      <div.pushRight>
                          #{exerciseRow (setExercises s) wPref}
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



currentUserWorkouts :: Handler [Workout]
currentUserWorkouts = do
    both <- currentUserExerciseList
    let exsByDay = filter (not . null) $ unsafeArrange2 (entityVal2 both) groupExercises
    return $ map (toWorkout) exsByDay
    where
        toSet :: [(Exercise,ExerciseType)] -> ExerciseSet
        toSet xs = ExerciseSet {
            setDate = exerciseStarted . leader $ xs,
            setType = type_ $ xs,
            setExercises = map fst xs
            }

        leader = fst . L.head
        type_  = snd . L.head

        toWorkout :: [(Exercise,ExerciseType)] -> Workout
        toWorkout = lift2Workout . toSets

        toSets :: [(Exercise,ExerciseType)] -> [ExerciseSet]
        toSets = reverse . map (toSet . reverse) . (L.groupBy ( (==) `on`  snd))

        lift2Workout :: [ExerciseSet] -> Workout 
        lift2Workout sets = Workout {
            workoutDate      = setDate . L.head $ sets,
            workoutExercises = sets
        }

groupExercises :: [Exercise] -> [[Exercise]]
groupExercises es = L.groupBy (  ( (<maxSecondsDiff) . ) . (diffUTCTime `on` exerciseStarted)) sorted
    where sorted = reverse $ L.sortBy (compare `on` exerciseStarted) es
          maxSecondsDiff = 400*60


-- These functions are unsafe. f must be solely a permutation function. I can't think
-- of anything better for now
unsafeArrange :: (Eq a, Functor f) => [(a,b)] -> ([a] -> f a) -> f (a,b)
unsafeArrange l f = fmap keyToPair result 
    where 
        result    = f $ map fst l
        keyToPair k = (k, fromJust $ lookup k l)

unsafeArrange2 :: (Eq a, Functor f) => [(a,b)] -> ([a] -> f (f a)) -> f (f (a,b))
unsafeArrange2 l f = (fmap.fmap) keyToPair result 
    where 
        result    = f $ map fst l
        keyToPair k = (k, fromJust $ lookup k l)

currentUserExerciseList = requireAuthId >>= exercisesForUserId

exercisesForUserId uid = runDB $ 
    E.select $ E.from $ \(e,et) -> do
    E.where_ ( (e E.^. ExerciseUserId E.==. E.val uid) E.&&. (e E.^. ExerciseKindId E.==. et E.^. ExerciseTypeId) )
    E.orderBy [E.desc (e E.^. ExerciseStarted)]
    return (e,et)


postWorkoutsR :: Handler Html
postWorkoutsR = error "undefined yet"


-- deletes everything and adds some data to DB
clear1 = do
    u <- requireAuthId
    t <- liftIO getCurrentTime
    _ <- runDB $ deleteWhere ([]::[Filter Exercise])
    _ <- runDB $ deleteWhere ([]::[Filter ExerciseType])
    squat <- runDB $ insert (ExerciseType "Squat" u WithWeight)
    benchPress <- runDB $ insert (ExerciseType "Bench Press" u WithWeight)
    barbellrow <- runDB $ insert (ExerciseType "Barbell row" u WithWeight)

    let daysBefore x = t { utctDay = (addDays (-x) (utctDay t)) }
    let yesterday    = daysBefore (1)
    let twoDays      = daysBefore (2)

    _ <- runDB $ insert $ Exercise u squat (Kilograms 120) 5 Nothing yesterday
    _ <- runDB $ insert $ Exercise u squat (Kilograms 120) 5 Nothing yesterday
    _ <- runDB $ insert $ Exercise u squat (Kilograms 120) 5 Nothing yesterday

    _ <- runDB $ insert $ Exercise u barbellrow (Kilograms 120) 5 Nothing yesterday
    _ <- runDB $ insert $ Exercise u barbellrow (Kilograms 120) 5 Nothing yesterday
    _ <- runDB $ insert $ Exercise u barbellrow (Kilograms 120) 5 Nothing yesterday

    _ <- runDB $ insert $ Exercise u benchPress (Kilograms 120) 5 Nothing yesterday
    _ <- runDB $ insert $ Exercise u benchPress (Kilograms 120) 5 Nothing yesterday
    _ <- runDB $ insert $ Exercise u benchPress (Kilograms 120) 5 Nothing yesterday

    _ <- runDB $ insert $ Exercise u squat (Kilograms 120) 5 Nothing twoDays
    _ <- runDB $ insert $ Exercise u squat (Kilograms 120) 5 Nothing twoDays
    _ <- runDB $ insert $ Exercise u squat (Kilograms 120) 5 Nothing twoDays

    _ <- runDB $ insert $ Exercise u barbellrow (Kilograms 120) 5 Nothing twoDays
    _ <- runDB $ insert $ Exercise u barbellrow (Kilograms 120) 5 Nothing twoDays
    _ <- runDB $ insert $ Exercise u barbellrow (Kilograms 120) 5 Nothing twoDays

    _ <- runDB $ insert $ Exercise u benchPress (Kilograms 120) 5 Nothing twoDays
    _ <- runDB $ insert $ Exercise u benchPress (Kilograms 120) 5 Nothing twoDays
    _ <- runDB $ insert $ Exercise u benchPress (Kilograms 120) 5 Nothing twoDays


    return ()

