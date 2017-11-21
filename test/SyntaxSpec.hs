{-# LANGUAGE OverloadedStrings #-}
module SyntaxSpec where

import           Syntax
import           Test.Hspec

spec :: Spec
spec = describe "SQL syntax parser" $ do
  it "parses 'SELECT foo, bar FROM ... WHERE ...' as a Select statement" $ do
    selectParser "SELECT foo, bar FROM ... WHERE ..." `shouldBe` Just (Select "foo, bar")
  it "parses 'SELECT ... FROM foo, bar WHERE ...' as a From statement" $ do
     fromParser "SELECT ... FROM foo, bar WHERE ..." `shouldBe` Just (From "foo, bar")
  it "parses 'SELECT ... FROM ... WHERE foo=bar as a Where statement" $ do
    whereParser "SELECT ... FROM ... WHERE foo=bar" `shouldBe` Just (Where "foo=bar")
  it "parses 'SELECT col1, col2 FROM tab1, tab2 WHERE contraints;' as a SelectStmt" $ do
    sqlParser "SELECT col1, col2 FROM tab1, tab2 WHERE constraints;" `shouldBe` SelectStmt (Select "col1, col2") (From "tab1, tab2") (Where "constraints;")
