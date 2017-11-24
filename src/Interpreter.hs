{-# LANGUAGE OverloadedStrings #-}
module Interpreter
    ( -- * Top-level CLI
      console
      -- * Interpreter
    , Command(..), interpret
    )
where

import           Data.Text
import           Syntax

data Command = Exit
             | Sql SqlStatement
               deriving (Eq, Show)

interpret :: Text -> Command
interpret ".exit"   = Exit
interpret statement = Sql (sqlParser statement)

console :: IO ()
console = do
  putStr "> "
  line <- getLine
  let output = interpret $ pack line
  case output of
    Exit     -> putStrLn "bye !"
    Sql _ -> do
      putStrLn "Parsed o7"
      console
