{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Jabara.Wunderlist.Client.Types (
    AccessToken
  , ClientId

  , UserId
  , ListId
  , TaskId
  , NoteId
  , Revision

  , fromWunderlistDay

  , Credential(..), credentialAccessToken, credentialClientId

  , Task(..), task_id, task_assignee_id, task_assigner_id, task_created_at
            , task_created_by_id, task_due_date, task_list_id, task_revision
            , task_starred, task_title
  , List(..), list_id , list_created_at, list_title
            , list_list_type, list_type, list_revision

  , ListUser(..), listUser_id, listUser_name, listUser_email
                , listUser_created_at

  , Note(..), note_id, note_task_id, note_content
            , note_created_at, note_updated_at
            , note_revision
) where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(String))
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.ByteString (ByteString)
import Data.Text (Text, pack, unpack)
import Data.Time (Day(..))
import Data.Time.Format (ParseTime(..), FormatTime(..)
                        , formatTime, parseTimeM, defaultTimeLocale)
import GHC.Base (mzero)
import GHC.Generics (Generic)
import Jabara.Util (omittedFirstCharLower)

type AccessToken = ByteString
type ClientId    = ByteString

type UserId   = Integer
type ListId   = Integer
type TaskId   = Integer
type NoteId   = Integer
type Revision = Int

data Credential = Credential {
    _credentialClientId    :: ClientId
  , _credentialAccessToken :: AccessToken
} deriving (Show, Read, Eq, Generic)
makeLenses ''Credential

newtype WunderlistDay = WunderlistDay {
    wunderlistDayValue :: Day
} deriving (Show, Read, Eq)

instance FromJSON WunderlistDay where
    parseJSON (String t) = fromWunderlistDay t
    parseJSON  _         = mzero
instance ToJSON WunderlistDay where
    toJSON d = String $ pack $ formatTime defaultTimeLocale "%Y-%m-%d" $ wunderlistDayValue d
instance ParseTime WunderlistDay where
    buildTime locale ss  = let d = buildTime locale ss
                           in  WunderlistDay d
instance FormatTime WunderlistDay where
    formatCharacter c =
        let mF = formatCharacter c
          in
            case mF of
              Nothing -> Nothing
              Just  f -> Just (\locale mb d -> f locale mb $ wunderlistDayValue d)

fromWunderlistDay :: (Monad m) => Text -> m WunderlistDay
fromWunderlistDay t = parseTimeM False defaultTimeLocale "%Y-%m-%d" (unpack t)

-- | REST APIの結果の型群.
data ListUser = ListUser {
    _listUser_id         :: UserId
  , _listUser_name       :: Text
  , _listUser_email      :: Text
  , _listUser_created_at :: Text
} deriving (Show, Read, Eq, Generic)
makeLenses ''ListUser
$(deriveJSON defaultOptions {
    fieldLabelModifier = omittedFirstCharLower "_listUser_"
} ''ListUser)

data Task = Task {
    _task_id            :: TaskId
  , _task_assignee_id   :: Maybe UserId
  , _task_assigner_id   :: Maybe UserId
  , _task_created_at    :: Text
  , _task_created_by_id :: UserId
  , _task_due_date      :: Maybe WunderlistDay
  , _task_list_id       :: ListId
  , _task_revision      :: Revision
  , _task_starred       :: Bool
  , _task_title         :: Text
} deriving (Show, Read, Eq, Generic)
makeLenses ''Task
$(deriveJSON defaultOptions {
    fieldLabelModifier = omittedFirstCharLower "_task_"
} ''Task)

data List = List {
    _list_id         :: ListId
  , _list_created_at :: Text
  , _list_title      :: Text
  , _list_list_type  :: Text
  , _list_type       :: Text
  , _list_revision   :: Revision
} deriving (Show, Read, Eq, Generic)
makeLenses ''List
$(deriveJSON defaultOptions {
    fieldLabelModifier = omittedFirstCharLower "_list_"
} ''List)

data Note = Note {
    _note_id         :: NoteId
  , _note_task_id    :: TaskId
  , _note_content    :: Text
  , _note_created_at :: Maybe Text
  , _note_updated_at :: Maybe Text
  , _note_revision   :: Revision
} deriving (Show, Read, Eq, Generic)
makeLenses ''Note
$(deriveJSON defaultOptions {
    fieldLabelModifier = omittedFirstCharLower "_note_"
} ''Note)

