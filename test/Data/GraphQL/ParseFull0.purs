module Test.Data.GraphQL.ParseFull0 where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (either)
import Effect.Exception (Error)
import Data.GraphQL.Parser as GP
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Text.Parsing.Parser (runParser, Parser)
import Text.Parsing.Parser.String (class StringLike)

parseCheck ∷ ∀ s t m. StringLike s ⇒ MonadThrow Error m ⇒ Show t ⇒ Eq t ⇒ Parser s t → s → m Unit
parseCheck parser toparse = either (fail <<< show) (\_ -> 1 `shouldEqual` 1) (runParser toparse parser)

schema =
  """type Tweet {
    id: ID!
    # The tweet text. No more than 140 characters!
    body: String
    # When the tweet was published
    date: Date
    # Who published the tweet
    Author: User
    # Views, retweets, likes, etc
    Stats: Stat
}

type User {
    id: ID!
    username: String
    first_name: String
    last_name: String
    full_name: String
    name: String @deprecated
    avatar_url: Url
}

type Stat {
    views: Int
    likes: Int
    retweets: Int
    responses: Int
}

type Notification {
    id: ID
    date: Date
    type: String
}

type Meta {
    count: Int
}

scalar Url
scalar Date

type Query {
    Tweet(id: ID!): Tweet
    Tweets(limit: Int, skip: Int, sort_field: String, sort_order: String): [Tweet]
    TweetsMeta: Meta
    User(id: ID!): User
    Notifications(limit: Int): [Notification]
    NotificationsMeta: Meta
}

type Mutation {
    createTweet (
        body: String
    ): Tweet
    deleteTweet(id: ID!): Tweet
    markTweetRead(id: ID!): Boolean
}""" ::
    String

testFullDoc ∷ forall e m. Monad m => Bind e => MonadThrow Error e => SpecT e Unit m Unit
testFullDoc =
  describe "test full doc" do
    it "should parse document correctly" do
      parseCheck GP.document schema
