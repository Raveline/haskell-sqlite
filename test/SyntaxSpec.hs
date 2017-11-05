{-# LANGUAGE OverloadedStrings #-}
module SyntaxSpec where

import Syntax
import Test.Hspec

spec :: Spec
spec = describe "SQL syntax parser" $ do
  it "parses 'select ...' as a select SQL statement" $ do
    parse "select foobar" `shouldBe` SelectStmt (Select "select foobar")
  it "parses 'inset into...' as an tnsert SQL statement" $ do
    parse "insert into foobar" `shouldBe` InsertStmt (Insert "insert into foobar")
  it "parses 'insert foobar' as an invalid sql statement" $ do
    parse "insert foobar" `shouldBe` Invalid "insert foobar"
