{-# LANGUAGE OverloadedStrings #-}
module SyntaxSpec where

import Syntax
import Test.Hspec

spec :: Spec
spec = describe "SQL intermediate syntax parser" $ do
  it "parses 'SELECT foo, bar FROM ... WHERE ...' as a Select statement" $ do
    selectParser "SELECT foo, bar FROM ... WHERE ..." `shouldBe` Select "foo, bar"
  it "parses 'SELECT ... FROM foo, bar WHERE ...' as a From statement" $ do
     fromParser "SELECT ... FROM foo, bar WHERE ..." `shouldBe` From "foo, bar"
  it "parses 'SELECT ... FROM ... WHERE foo=bar as a Where statement" $ do
    whereParser "SELECT ... FROM ... WHERE foo=bar" `shouldBe` Where "foo=bar"