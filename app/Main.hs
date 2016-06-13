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
import           Data.Maybe (isJust, fromMaybe)
import           Data.Scientific (coefficient)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Jabara.Util (listToMap)
import           System.Environment

data CsvItemValueSetting =
    ValueFromJSON {
        _valueFromJSONKey :: T.Text
    }
    | GoogleDate {
        _googleDateKey :: T.Text
    }
    | FixedText {
        _fixedTextValue :: T.Text
    }
    | CustomValue {
        _customize :: (Value -> M.Map WunderlistId ListUser -> T.Text)
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
                    CsvItemSetting "Subject"       $ ValueFromJSON "title"
                  , CsvItemSetting "Start Date"    $ GoogleDate    "due_date"
                  -- , CsvItemSetting "End Date"      $ GoogleDate    "due_date"
                  , CsvItemSetting "All Day Event" $ FixedText     "True"
                  , CsvItemSetting "Private"       $ FixedText     "False"
                  , CsvItemSetting "Description"   $ CustomValue (\val users ->
                        fromMaybe "" $ do
                            wid  <- (Just val) ^. key "assignee_id"
                            user <- M.lookup wid users
                            pure $ encDC $ user^.listUser_name
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

listIdFromEnv :: IO WunderlistId
listIdFromEnv = getEnv "LIST_ID" >>= pure . read

hasValue :: T.Text -> Value -> Bool
hasValue prop val = isJust $ ((Just val) ^. key prop ::Maybe Value)

buildCsv :: M.Map WunderlistId ListUser -> CsvItemSettings -> [Value] -> T.Text
buildCsv users settings values = T.unlines (header:map valueToLine values)
  where
    header :: T.Text
    header = T.intercalate sep $ map _csvItemSettingLabel settings

    valueToLine :: Value -> T.Text
    valueToLine value = T.intercalate sep $
        map (textValue value) settings

    textValue :: Value -> CsvItemSetting -> T.Text
    textValue val@(Object _) (CsvItemSetting _ (ValueFromJSON prop)) = textValue' prop val
    textValue val@(Object _) (CsvItemSetting _ (CustomValue f))      = f val users
    textValue val@(Object _) (CsvItemSetting _ (GoogleDate  prop))   = fromMaybe "" $ do
        t <- (Just val) ^. key prop
        d <- fromWunderlistDay t
        pure $ T.pack $ formatTime defaultTimeLocale "%d/%m/%Y" d
    textValue _              (CsvItemSetting _ (FixedText   t))      = t
    textValue _              _                                       = error "error."

    textValue' ::T.Text -> Value -> T.Text
    textValue' prop val = case (Just val) ^. key prop of
        Nothing  -> ""
        Just (String t) -> encDC t
        Just (Number n) -> T.pack $ show $ coefficient n
        Just (Bool   b) -> if b then "True"
                                else "False"
        Just (Null    ) -> ""
        Just (_       ) -> error $ "error '" ++ (show val) ++ "'"

main :: IO ()
main = do
    listId <- listIdFromEnv
    cre    <- credentialFromEnv
    tasks  <- getTasksAsJson cre listId
                 >>= pure . filter (hasValue "due_date")
    users  <- getListUsers cre >>= pure . listToMap _listUser_id
    T.putStrLn $ buildCsv users defaultSettings tasks
