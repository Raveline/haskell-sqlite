{-# LANGUAGE OverloadedStrings #-}
module Test where

import Data.Maybe
import Data.Monoid
import Data.Text
import Text.Parsec
import Text.Parsec.Char

newtype Select = Select Text deriving (Eq, Show)
newtype From = From Text deriving (Eq, Show)
newtype Where = Where Text deriving (Eq, Show)
newtype Unknown = Unknown Text deriving (Eq, Show)
data SqlStatement = SelectStmt Select From Where
                  | InvalidStmt Text
   deriving (Eq, Show)

selectParser :: Text -> Maybe Select
selectParser statement = do
   case parse parser "<STDIN>" statement of
      Left err     -> Nothing
      Right parsed -> Just (Select $ pack parsed)
   where
    parser = do
        string "SELECT"
        space
        condition <- untilFrom
        return condition
    untilFrom = manyTill anyChar (try (string " FROM"))

fromParser :: Text -> Maybe From
fromParser statement = do
    case parse parser "<STDIN>" statement of
        Left err     -> Nothing
        Right parsed -> Just (From $ pack parsed)
    where
     parser = do
        manyTill anyChar (try (string "FROM"))
        space
        condition <- untilWhere
        return condition
     untilWhere = manyTill anyChar (try (string " WHERE"))

whereParser :: Text -> Maybe Where
whereParser statement = do
    case parse parser "<STDIN>" statement of
        Left err     -> Nothing
        Right parsed -> Just (Where $ pack parsed)
    where
     parser = do
        manyTill anyChar (try (string "WHERE"))
        space
        condition <- many anyChar
        return condition