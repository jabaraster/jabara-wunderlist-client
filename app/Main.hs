{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Jabara.Wunderlist.Client

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8 as B
import           Data.Scientific (coefficient)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           GHC.Generics (Generic)
import           System.Environment

data CsvItemSetting = CsvItemSetting {
    _csvItemSettingLabel :: T.Text
  , _csvItemSettingKey   :: T.Text
} deriving (Show, Read, Eq, Generic)
makeLenses ''CsvItemSetting

type CsvItemSettings = [CsvItemSetting]

defaultSettings :: CsvItemSettings
defaultSettings = [
                    CsvItemSetting "Id"          "id"
                  , CsvItemSetting "Subject"     "title"
                  , CsvItemSetting "Start Date"  "due_date"
                  , CsvItemSetting "Assignee Id" "assignee_id"
                  , CsvItemSetting "Assigner Id" "assigner_id"
                  ]

credentialFromEnv :: IO Credential
credentialFromEnv = do
    clientId    <- getEnv "CLIENT_ID"
    accessToken <- getEnv "ACCESS_TOKEN"
    pure $ Credential (B.pack clientId) (B.pack accessToken)

listIdFromEnv :: IO WunderlistId
listIdFromEnv = getEnv "LIST_ID" >>= pure . read

main :: IO ()
main = do
    listId <- listIdFromEnv
    cre    <- credentialFromEnv
    tasks  <- getTasksAsJson cre listId
    T.putStrLn $ buildCsv defaultSettings tasks

buildCsv :: CsvItemSettings -> [Value] -> T.Text
buildCsv settings values = T.unlines (header:map valueToLine values)
  where
    header :: T.Text
    header = T.intercalate "\t" $ map _csvItemSettingLabel settings

    valueToLine :: Value -> T.Text
    valueToLine value = T.intercalate "\t" $
        map (\s -> textValue (s^.csvItemSettingKey) value) settings

    textValue ::T.Text -> Value -> T.Text
    textValue prop val = case (Just val) ^. key prop of
        Nothing  -> ""
        Just (String t) -> "\"" `T.append` t `T.append` "\""
        Just (Number n) -> T.pack $ show $ coefficient n
        Just (Bool   b) -> T.pack $ show b
        Just (Null    ) -> ""
        Just (_       ) -> error $ "error '" ++ (show val) ++ "'"
