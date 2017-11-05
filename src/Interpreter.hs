{-# LANGUAGE OverloadedStrings #-}
module Interpreter
    ( -- * Top-level CLI
      console
      -- * Interpreter
    , Command(..), interpret
    )
where

import Data.Text
import Control.Monad(forever)
import Data.Monoid((<>))

import Syntax

data Command = Exit
             | SqlStatement Statement
  deriving (Eq, Show)

interpret :: Text -> Command
interpret ".exit" = Exit
interpret statement = SqlStatement (parse statement)

console :: IO ()
console = do
  putStr "> "
  line <- getLine
  let output = interpret $ pack line
  case output of
    Exit              -> putStrLn "bye !"
    SqlStatement stmt -> do
      putStrLn "Wow such statement !"
      console
