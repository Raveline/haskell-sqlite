{-# LANGUAGE OverloadedStrings #-}
module Syntax
    (
      parse,
      Statement(..), Select(..), Insert(..)
    )
where

import qualified Data.List as DL
import qualified Data.List.Split as DLS
import qualified Data.Text as DT

data Select = Select DT.Text deriving (Eq, Show)
data From = From DT.Text deriving (Eq, Show)
data Where = Where DT.Text deriving (Eq, Show)
data Insert = Insert DT.Text deriving (Eq, Show)
data Values = Values DT.Text deriving (Eq, Show)

data Statement = SelectStmt Select From Where
               | InsertStmt Insert Values
               | Invalid DT.Text
  deriving (Eq, Show)

-- SPEC
parse :: DT.Text -> Statement
parse = undefined;
-- parse statement
--   | isPrefixOf "select " statement      = SelectStmt (Select statement)
--   | isPrefixOf "insert into " statement = InsertStmt (Insert statement)
--   | otherwise                           = Invalid statement


trimSpaces :: String -> String
trimSpaces s = DT.unpack $ DT.replace " " "" (DT.pack s)

trimSemicolon :: String -> String
trimSemicolon s = DT.unpack $ DT.replace ";" "" (DT.pack s)

-- SPEC
-- Select "column1 as A, column 2 as B"
-- From "table1, table2"
-- Where "table1.col = table2.col"
sql = "select column1 as A, column 2 as B from table1, table2 where table1.col = table2.col;"

whereClause :: String -> String
whereClause sqlStatement = trimmedWhere
  where trimmedWhere = trimSemicolon withoutSpaces
        withoutSpaces = trimSpaces uglyWhere
        uglyWhere = last splitOnWhere
        splitOnWhere = DLS.split (DLS.onSublist "where") sqlStatement
