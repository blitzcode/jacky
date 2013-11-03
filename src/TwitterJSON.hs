
{-# LANGUAGE OverloadedStrings #-}

module TwitterJSON ( CreatedAtTime(..)
                   , User(..)
                   , Tweet(..)
                   , StreamMessage(..)
                   , twitterUserStreamURL
                   , twitterStatusesRandomStreamURL
                   , twitterStatusesFilterStreamURL
                   , twitterHomeTimeline
                   ) where

import qualified Data.Text as T
import Data.ByteString as B
import Data.Int
import Data.Aeson
import Data.Time.Clock
import qualified Data.HashMap.Strict as HM
import Data.Time
import Control.Applicative
import System.Locale (defaultTimeLocale)

-- Data structures and parsers for dealing with data returned from the Twitter
-- REST and streaming APIs

-- Twitter API endpoints
twitterUserStreamURL,
    twitterStatusesRandomStreamURL,
    twitterStatusesFilterStreamURL,
    twitterHomeTimeline
    :: String
twitterUserStreamURL           = "https://userstream.twitter.com/1.1/user.json"
twitterStatusesRandomStreamURL = "https://stream.twitter.com/1.1/statuses/sample.json"
twitterStatusesFilterStreamURL = "https://stream.twitter.com/1.1/statuses/filter.json"
twitterHomeTimeline            = "https://api.twitter.com/1.1/statuses/home_timeline.json"

newtype CreatedAtTime = CreatedAtTime { fromCreatedAtTime :: UTCTime } deriving (Show)

instance FromJSON CreatedAtTime where
    parseJSON (String t) = {-# SCC parseCreatedAtTime #-} 
        case parseTime defaultTimeLocale "%a %b %d %H:%M:%S %z %Y" $ T.unpack t of
            Just d -> pure $ CreatedAtTime d
            _      -> fail $ "Could not parse 'created at' time" ++ T.unpack t
    parseJSON _ = fail "CreatedAtTime: Not a string"

data User = User { usrID              :: Int64
                 , usrName            :: T.Text
                 , usrScreenName      :: T.Text
                 , usrURL             :: Maybe B.ByteString
                 , usrDesc            :: Maybe T.Text
                 , usrFollowersCnt    :: Int
                 , usrStatusesCnt     :: Int
                 , usrCreatedAt       :: CreatedAtTime
                 , usrVerified        :: Bool
                 , usrLang            :: B.ByteString
                 , usrProfileImageURL :: B.ByteString
                 } deriving (Show)

instance FromJSON User where
    parseJSON (Object o) =
        User <$> o .:  "id"
             <*> o .:  "name"
             <*> o .:  "screen_name"
             <*> o .:? "url"
             <*> o .:? "description"
             <*> o .:  "followers_count"
             <*> o .:  "statuses_count"
             <*> o .:  "created_at"
             <*> o .:  "verified"
             <*> o .:  "lang"
             <*> o .:  "profile_image_url"
    parseJSON _ = fail "Expected Object"

data Tweet = Tweet { twCreatedAt             :: CreatedAtTime
                   , twID                    :: Int64
                   , twText                  :: T.Text
                   , twUser                  :: User
                   , twSource                :: T.Text
                   , twReplyToStatusID       :: Maybe Int64
                   , twReplyToUserID         :: Maybe Int64
                   , twReplyToUserScreenName :: Maybe T.Text
                   , twFilterLevel           :: Maybe B.ByteString
                   , twLang                  :: B.ByteString
                   , twFavoriteCount         :: Maybe Int
                   , twRetweetCount          :: Maybe Int
                   , twRetweetedStatus       :: Maybe Tweet -- Contains original
                   } deriving (Show)

-- Newtype for Tweets sorted by different criteria

{-
                   --, TweetByID(..)
                   --, TweetByCreatedAt(..)

import Data.Function (on)

newtype TweetByID = TweetByID { fromTweetByID :: Tweet }
instance Eq TweetByID where
    a == b = compare a b == EQ
instance Ord TweetByID where
    compare = compare `on` (twID . fromTweetByID)

newtype TweetByCreatedAt = TweetByCreatedAt { fromTweetByCreatedAt :: Tweet }
instance Eq TweetByCreatedAt where
    a == b = compare a b == EQ
instance Ord TweetByCreatedAt where
    compare a b =
        let byTime = compare `on` (fromCreatedAtTime . twCreatedAt . fromTweetByCreatedAt)
            byID   = compare `on` (twID . fromTweetByCreatedAt)
        in  case byTime a b of -- Compare on time, then ID to disambiguate
            EQ -> byID a b
            LT -> LT
            GT -> GT
-}

instance FromJSON Tweet where
    parseJSON (Object o) =
        Tweet <$> o .:  "created_at"
              <*> o .:  "id"
              <*> o .:  "text"
              <*> o .:  "user"
              <*> o .:  "source"
              <*> o .:? "in_reply_to_status_id"
              <*> o .:? "in_reply_to_user_id"
              <*> o .:? "in_reply_to_screen_name"
              <*> o .:? "filter_level"
              <*> o .:  "lang"
              <*> o .:? "favorite_count"
              <*> o .:? "retweet_count"
              <*> o .:? "retweeted_status"
    parseJSON _ = fail "Expected Object"

-- TODO: The record fields in the different value constructors are a bit nasty,
--       maybe we can make this more type safe with DataKinds or by breaking out
--       more of them into separate records like we already did for Tweet
data StreamMessage = SMDelete { smdelID     :: Int64
                              , smdelUserID :: Int64
                              }
                   | SMScrubGeo { smsgUserID       :: Int64
                                , smsgUpToStatusID :: Int64
                                }
                   | SMLimit Int
                   | SMStatusWithheld
                   | SMUserWithheld
                   | SMDisconnect { smdcCode       :: Int
                                  , smdcStreamName :: String
                                  , smdcReason     :: String
                                  }
                   | SMWarning { smwCode        :: B.ByteString
                               , smwMessage     :: B.ByteString
                               , smwUserID      :: Maybe Int64
                               , smwPercentFull :: Maybe Int
                               }
                   | SMFriends [Int64]
                   | SMFriendsStr
                   | SMEvent { smeTargetUser :: Maybe Int64
                             , smeSourceUser :: Maybe Int64
                             , smeEventName  :: B.ByteString
                             , smeTargetObj  :: Maybe B.ByteString
                             , smeCreatedAt  :: CreatedAtTime
                             }
                   | SMTweet Tweet
                   | SMParseError B.ByteString
                   | SMBytesReceived Int -- Just for statistics
                     deriving (Show)

instance FromJSON StreamMessage where
    parseJSON j =
        -- Try to identify which type of message we've got, most of the
        -- non-tweet ones have a single top level object we can identify
        -- by name
        case j of
            -- TODO: Maybe this could be expressed nicer using Alternative and <|>
            (Object o) | HM.member "text" o -> -- Tweet
                         SMTweet <$> parseJSON j
                       | HM.member "delete" o ->
                         do val <- o .: "delete" >>= (.: "status")
                            SMDelete <$> val .: "id" <*> val .: "user_id"
                       | HM.member "scrub_geo" o ->
                         do val <- o .: "scrub_geo"
                            SMScrubGeo <$> val .: "user_id" <*> val .: "up_to_status_id"
                       | HM.member "limit" o ->
                         do val <- o .: "limit"
                            SMLimit <$> val .: "track"
                       | HM.member "status_withheld" o -> return SMStatusWithheld -- TODO: Implement
                       | HM.member "user_withheld"   o -> return SMUserWithheld   --       ...
                       | HM.member "friends_str"     o -> return SMFriendsStr     --       ...
                       | HM.member "friends" o ->
                            SMFriends <$> o .: "friends"
                       | HM.member "disconnect" o ->
                         do val <- o .: "disconnect"
                            SMDisconnect <$> val .: "code"
                                         <*> val .: "stream_name"
                                         <*> val .: "reason"
                       | HM.member "warning" o ->
                         do val <- o .: "warning"
                            SMWarning <$> val .:  "code"
                                      <*> val .:  "message"
                                      <*> val .:? "user_id"
                                      <*> val .:? "percent_full"
                       | HM.member "event" o -> -- Event type object
                         do val <- o .: "warning"
                            SMEvent <$> val .:? "target"
                                    <*> val .:? "source"
                                    <*> val .:  "event"
                                    <*> val .:? "target_object"
                                    <*> val .:  "created_at"
                       -- We don't return SMParseError directly, instead we fail
                       -- and let the caller do the wrapping of the message
                       | otherwise -> fail $ "Unknown Object - " ++ show j
            _ -> fail $ "Expected Object - " ++ show j

