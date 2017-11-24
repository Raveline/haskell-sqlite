{-# LANGUAGE OverloadedStrings #-}
module Syntax
    (
      sqlParser, selectParser, fromParser, whereParser,
      SqlStatement(..), Select(..), From(..), Where(..)
    )
where

import           Data.Text
import           Text.Parsec

newtype Select = Select Text
                 deriving (Eq, Show)
newtype From = From Text
               deriving (Eq, Show)
newtype Where = Where Text
                deriving (Eq, Show)
data SqlStatement = SelectStmt Select From Where
                  | InvalidStmt Text
                    deriving (Eq, Show)

mapToSqlStatement :: Text -> Maybe Select -> Maybe From -> Maybe Where -> SqlStatement
mapToSqlStatement "" _ _ _                       = InvalidStmt "Not a SQL statement"
mapToSqlStatement sql Nothing _ _                = InvalidStmt "Not a SQL statement"
mapToSqlStatement sql _ Nothing _                = InvalidStmt "Not a SQL statement"
mapToSqlStatement sql _ _ Nothing                = InvalidStmt "Not a SQL statement"
mapToSqlStatement sql (Just s) (Just f) (Just w) = SelectStmt s f w

sqlParser :: Text -> SqlStatement
sqlParser sql = mapToSqlStatement sql selectPart fromPart wherePart
    where
        selectPart = selectParser sql
        fromPart = fromParser sql
        wherePart = whereParser sql


selectParser :: Text -> Maybe Select
selectParser statement = do
   case parse parser "<STDIN>" (unpack statement) of
      Left _       -> Nothing
      Right parsed -> Just (Select $ strip $ pack parsed)
   where
    parser = string "SELECT" *> space *> untilFrom

fromParser :: Text -> Maybe From
fromParser statement = do
    case parse parser "<STDIN>" (unpack statement) of
        Left _       -> Nothing
        Right parsed -> Just (From $ strip $ pack parsed)
    where
     parser = untilFrom *> many1 space *> untilWhere

whereParser :: Text -> Maybe Where
whereParser statement = do
    case parse parser "<STDIN>" (unpack statement) of
        Left _       -> Nothing
        Right parsed -> Just (Where $ pack parsed)
    where
     parser = untilWhere *> space *> many anyChar

untilKeyword :: String -> Parsec String () String
untilKeyword keyword = manyTill anyChar (try (string keyword))

untilWhere, untilFrom :: Parsec String () String
untilFrom = untilKeyword "FROM"
untilWhere = untilKeyword "WHERE"
