{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Jabara.Wunderlist.Client

import           Control.Lens
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M (Map, lookup)
import           Data.Maybe (isJust, fromMaybe, catMaybes)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Jabara.Util (listToMap)
import           System.Environment

type ListUserDb = M.Map UserId ListUser
type NoteDb     = M.Map TaskId Note

data CsvItemValueSetting =
      Title       (ListUserDb -> Task -> T.Text)
    | GoogleDate  (Task -> T.Text)
    | FixedText   T.Text
    | Description (NoteDb -> Task -> T.Text)
makeLenses ''CsvItemValueSetting

data CsvItemSetting = CsvItemSetting {
    csvItemSettingLabel :: T.Text
  , csvItemValueSetting :: CsvItemValueSetting
}

type CsvItemSettings = [CsvItemSetting]

defaultSettings :: CsvItemSettings
defaultSettings = [
      CsvItemSetting "Subject"       $ Title (\users task ->
          let title            = task^.task_title
              assigneeUserName = getAssigneeUserName users task
          in encDC $ T.intercalate " - " $ catMaybes [Just title, assigneeUserName]
          )
    , CsvItemSetting "Start Date"    $ GoogleDate (\task ->
          fromMaybe "" $ do
              d <- task^.task_due_date
              pure $ T.pack $ formatTime defaultTimeLocale "%d/%m/%Y" d
          )
    , CsvItemSetting "All Day Event" $ FixedText "True"
    , CsvItemSetting "Private"       $ FixedText "False"
    , CsvItemSetting "Description"   $ Description (\notes task ->
          fromMaybe "" $ do
              tid  <- pure $ task^.task_id
              note <- M.lookup tid notes
              pure $ note^.note_content
          )
    ]

sep :: T.Text
sep = ","

encDC :: T.Text -> T.Text
encDC t = "\"" `T.append` t `T.append` "\""

credentialFromEnv :: IO Credential
credentialFromEnv = do
    clientId    <- getEnv "CLIENT_ID"
    accessToken <- getEnv "ACCESS_TOKEN"
    pure $ Credential (B.pack clientId) (B.pack accessToken)

listIdFromEnv :: IO ListId
listIdFromEnv = getEnv "LIST_ID" >>= pure . read

buildCsv :: ListUserDb -> NoteDb -> CsvItemSettings -> [Task] -> T.Text
buildCsv users notes settings values = T.unlines (header:map taskToLine values)
  where
    header :: T.Text
    header = T.intercalate sep $ map csvItemSettingLabel settings

    taskToLine :: Task -> T.Text
    taskToLine task = T.intercalate sep $ map (textValue task) settings

    textValue :: Task -> CsvItemSetting -> T.Text
    textValue task (CsvItemSetting _ (Title       f)) = f users task
    textValue task (CsvItemSetting _ (GoogleDate  f)) = f task
    textValue _    (CsvItemSetting _ (FixedText   t)) = t
    textValue task (CsvItemSetting _ (Description f)) = f notes task

getAssigneeUserName :: ListUserDb -> Task -> Maybe T.Text
getAssigneeUserName users task = do
    uid <- task^.task_assignee_id
    user <- M.lookup uid users
    pure $ user^.listUser_name

main :: IO ()
main = do
    listId <- listIdFromEnv
    cre    <- credentialFromEnv
    tasks  <- getTasks cre listId
                 >>= pure . filter (isJust . _task_due_date)
    users  <- getListUsers cre >>= pure . listToMap _listUser_id
    notes  <- getListNotes cre listId >>= pure . listToMap _note_task_id
    T.putStrLn $ buildCsv users notes defaultSettings tasks
