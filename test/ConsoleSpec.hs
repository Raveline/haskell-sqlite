{-# LANGUAGE OverloadedStrings #-}
module ConsoleSpec where

import Interpreter
import Syntax
import Test.Hspec

spec :: Spec
spec = describe "SQL Mini Interpreter" $ do
  it "interprets '.exit' as Exit command" $ do
    interpret ".exit" `shouldBe` Exit
  it "interprets unknown string  as an Invalid sql statement command" $ do
    interpret "foo" `shouldBe` SqlStatement (Invalid "foo")
