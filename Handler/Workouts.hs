{-# LANGUAGE OverloadedStrings #-}

module Handler.Workouts where

import Import
import Yesod.Auth
import Data.Time
import Control.Arrow
import qualified Database.Esqueleto as E

getWorkoutsR :: Handler Html
getWorkoutsR = do
   userId <- requireAuthId 
   exercises <- runDB $ selectList [ExerciseUserId ==. userId] []
   (logNewExerciseFormWidget, enctype) <- generateFormPost (logExerciseForm userId)
   defaultLayout $ do
       workouts
       [whamlet|
           <div class="col-lg-6">
              <form method=post action=@{WorkoutsR} class="form-horizontal">
                  ^{logNewExerciseFormWidget}
        |]


workouts :: Widget
workouts = do
    both <- handlerToWidget $ currentUserWorkouts
    [whamlet|
        <p> Your past exercises:
        $forall t <- both
            <div.well>
                #{show t}
    |]


{-currentUserWorkouts = do-}
    {-userId <- requireAuthId -}
    {-exerciseEntities <- runDB $ selectList [ExerciseUserId ==. userId] []-}
    {-let exTypeIds = map (exerciseExerciseType . entityVal) exerciseEntities-}
    {-exerciseTypes <- runDB $ selectList [ExerciseTypeId <-. exTypeIds] []-}
    {-let l = map (\(Entity k v) -> (k,v)) exerciseTypes-}
    {-let l' = map (flip lookup $ l) exTypeIds-}
    {-return $ zip exerciseEntities l'-}
 


{-currentUserWorkouts = do-}
    {-userId <- requireAuthId -}
    {-exerciseEntities <- runDB $ selectList [ExerciseUserId ==. userId] []-}
    {-let exTypeIds = map (exerciseExerciseType . entityVal) exerciseEntities-}
    {-exerciseTypes <- runDB $ selectList [ExerciseTypeId <-. exTypeIds] []-}
    {-return $ joinTables exerciseExerciseType exerciseEntities exerciseTypes-}
   
 
currentUserWorkouts = requireAuthId >>= (runDB . exercisesForUserId)
    {-uid <- requireAuthId-}
    {-k <- runDB (exercisesForUserId uid)-}
    {-return $ k-}

{-joinTables :: (a -> Key b) -> [Entity a] -> [Entity b] -> [(Entity a, Maybe (Entity b))]-}
{-joinTables f as bs =-}
    {-let keys   = map (f . entityVal) as-}
        {-bMap   = map (\e@(Entity k _) -> (k,e)) bs-}
        {-rest   = map (flip lookup $ bMap) keys-}
    {-in zip as rest -}



{-exercisesForUserId :: t-}
exercisesForUserId uid = do
    E.select $ E.from $ \(e,et) -> do
    E.where_ ( (e E.^. ExerciseUserId E.==. E.val uid) E.&&. (e E.^. ExerciseKindId E.==. et E.^. ExerciseTypeId) )
    return (e,et)

{-test = do-}
    {-people <- E.select $ -}
              {-E.from $ \a -> return a-}
    {-liftIO $ mapM_ (putStrLn . show . exerciseWeight . entityVal) people-}


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
