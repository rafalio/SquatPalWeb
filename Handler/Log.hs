{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Log where

import Import
import Yesod.Auth
import Control.Arrow
import Data.Time
import qualified Data.Text.Read
import GHC.Generics

getLogR :: Handler Html
getLogR = do
    (logNewExerciseFormWidget, _) <- generateFormPost logExerciseFormM
    defaultLayout $ do
        setTitle "SquatPal | Log your workout"
        toWidget [lucius|
            form#logExercise div{
                padding: 5px; 
            }
        |]
        [whamlet|
        <h3> Log your exercise
        <form.form-inline #logExercise method=post action=@{LogR}>
            <div.row>
                ^{logNewExerciseFormWidget}
                 <div.form-group>
                     <button.btn.btn-success>Log it!
        |]


-- what I get back from a JSON post, have to convert
-- to Exercise

data ExerciseAttr = ExerciseAttr {
  attrReps :: Int,
  attrKindId :: ExerciseTypeId,
  attrWeight     :: Weight,
  attrStarted    :: UTCTime
} deriving (Eq,Show,Generic)

instance ToJSON ExerciseAttr
instance FromJSON ExerciseAttr


exerciseAttrToExercise :: UserId -> ExerciseAttr -> Exercise
exerciseAttrToExercise uid exAtr = Exercise {
  exerciseUserId = uid,
  exerciseKindId = attrKindId exAtr,
  exerciseWeight = attrWeight exAtr,
  exerciseReps   = attrReps exAtr,
  exerciseNotes  = Nothing,
  exerciseStarted = attrStarted exAtr
}

postLogR :: Handler TypedContent
postLogR = selectRep $ do
    provideRep $ do
      ((result, _),_) <- runFormPost logExerciseFormM
      case result of
          FormSuccess newExercise -> do
              _ <- runDB $ insert newExercise
              setMessageT MsgSuccess "Succesfuly added your exercise!"
          FormFailure ts -> setMessageT MsgError (toHtml . mconcat $ ts)
          _ -> setMessageT MsgError "There was a problem saving your exercise"
      redirect LogR :: Handler Html
    provideRep $ do
        uid <- requireAuthId
        exercise <- return . (exerciseAttrToExercise uid) =<< requireJsonBody
        liftIO (putStrLn.show $ exercise)
        runDB $ insert exercise
        return $ (object ["success" .= True])

logExerciseFormM :: Form Exercise
logExerciseFormM extra = do

    uid  <- lift requireAuthId
    user <- lift requireUser

    (typeRes, typeView)      <- mreq (selectField $ exerciseTypes uid) (bfs ("Exercise Type" :: Text)) Nothing
    (weightRes, weightView)  <- mreq exWeightField (bfs ("Weight"::Text)) Nothing
    (repsRes, repsView)      <- mreq (checkBool (> 0) ("Number of reps must be positive"::Text) intField)
                                     (bfs ("Reps"::Text))
                                     Nothing

    time <- lift (liftIO getCurrentTime)
    let weightPref = show . userWeightPref $ user

    let logRes = Exercise <$> 
            pure uid <*> 
            typeRes <*> 
            weightRes <*> 
            repsRes <*> 
            pure Nothing <*> 
            pure time

    let widget = do
        toWidget [lucius|
            ##{fvId typeView}{
                width: 200px;
            }
           #weightDiv{
                width:150px;
            }
           #repsDiv{
               width:150px;
           }
       |]
        [whamlet|
          #{extra}
          <div.form-group>
              ^{fvInput typeView}
          <div.form-group #weightDiv>
              <div.input-group>
                  ^{fvInput weightView}
                  <span.input-group-addon>#{weightPref}
          <span>
              X
          <div.form-group #repsDiv>
              <div.input-group>
                  ^{fvInput repsView}
                  <span.input-group-addon>reps
        |]
    return (logRes, widget)
    where
        exerciseTypes uid = do
            exercisesForUser <- runDB $ selectList [ExerciseTypeCreatedBy ==. uid] []
            optionsPairs $ map ((exerciseTypeName . entityVal) &&& entityKey) exercisesForUser



exWeightField :: Field (HandlerT App IO) Weight
exWeightField = checkBool (>= 0) ("The weight must be non-negative"::Text) $ Field
    {
        fieldParse  = \t _f -> do
            u <- requireUser 
            case t of
                [v] ->
                    case Data.Text.Read.double v of
                        Right (a,"") -> case (userWeightPref u) of
                            Kg  -> f a 
                            Lbs -> f . lbs2kg $ a 
                            where f = return . Right . Just . Kilograms
                        _ -> return . Left . SomeMessage $ ("The weight must be a number!"::Text)
                _  -> return . Left . SomeMessage $ ("Error"::Text)

      , fieldView = \theId name attrs val isReq -> toWidget
        [hamlet|$newline never
            <input id="#{theId}" name="#{name}" *{attrs} :isReq:required>
        |]
      , fieldEnctype = UrlEncoded
    }
