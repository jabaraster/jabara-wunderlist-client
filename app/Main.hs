{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Jabara.Wunderlist.Client

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M (Map, lookup)
import           Data.Maybe (isJust, fromMaybe, catMaybes)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Jabara.Util (listToMap)
import           System.Environment

data CsvItemValueSetting =
    Title
    | Description
    | GoogleDate {
        _googleDateKey :: T.Text
    }
    | FixedText {
        _fixedTextValue :: T.Text
    }
makeLenses ''CsvItemValueSetting

data CsvItemSetting = CsvItemSetting {
    _csvItemSettingLabel :: T.Text
  , _csvItemValueSetting :: CsvItemValueSetting
}
makeLenses ''CsvItemSetting

type CsvItemSettings = [CsvItemSetting]

defaultSettings :: CsvItemSettings
defaultSettings = [
                    CsvItemSetting "Subject"       $ Title
                  , CsvItemSetting "Start Date"    $ GoogleDate    "due_date"
                  , CsvItemSetting "All Day Event" $ FixedText     "True"
                  , CsvItemSetting "Private"       $ FixedText     "False"
                  , CsvItemSetting "Description"   $ Description
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

hasValue :: T.Text -> Value -> Bool
hasValue prop val = isJust $ ((Just val) ^. key prop ::Maybe Value)

buildCsv :: M.Map UserId ListUser -> M.Map TaskId Note -> CsvItemSettings -> [Value] -> T.Text
buildCsv users notes settings values = T.unlines (header:map valueToLine values)
  where
    header :: T.Text
    header = T.intercalate sep $ map _csvItemSettingLabel settings

    valueToLine :: Value -> T.Text
    valueToLine value = T.intercalate sep $
        map (textValue value) settings

    textValue :: Value -> CsvItemSetting -> T.Text
    textValue val@(Object _) (CsvItemSetting _ Title) =
        let title    = (Just val)^.key "title"
            userName = getUserName val
        in  T.intercalate " - " $ catMaybes [title, userName]
    textValue val@(Object _) (CsvItemSetting _ Description) =
        encDC $ T.intercalate "\n" $ catMaybes [getUserName val, getDescription val]
    textValue val@(Object _) (CsvItemSetting _ (GoogleDate  prop)) = fromMaybe "" $ do
        t <- (Just val) ^. key prop
        d <- fromWunderlistDay t
        pure $ T.pack $ formatTime defaultTimeLocale "%d/%m/%Y" d
    textValue _ (CsvItemSetting _ (FixedText t)) = t
    textValue _ _                                = error "error."

    getUserName :: Value -> Maybe T.Text
    getUserName task = do
        uid  <- (Just task) ^. key "assignee_id"
        user <- M.lookup uid users
        pure $ user^.listUser_name

    getDescription :: Value -> Maybe T.Text
    getDescription task = do
        tid  <- (Just task) ^. key "id"
        note <- M.lookup tid notes
        pure $ note^.note_content

main :: IO ()
main = do
    listId <- listIdFromEnv
    cre    <- credentialFromEnv
    tasks  <- getTasksAsJson cre listId
                 >>= pure . filter (hasValue "due_date")
    users  <- getListUsers cre >>= pure . listToMap _listUser_id
    notes  <- getListNotes cre listId >>= pure . listToMap _note_task_id
    T.putStrLn $ buildCsv users notes defaultSettings tasks
