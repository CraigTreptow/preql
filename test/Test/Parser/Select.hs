{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLists          #-}
{-# LANGUAGE OverloadedStrings        #-}

module Test.Parser.Select where

import qualified Preql.QuasiQuoter.Select.Lex    as L
import           Preql.QuasiQuoter.Select.Parser (parseSelect)
import           Preql.QuasiQuoter.Select.Syntax

import           Data.List.NonEmpty              (NonEmpty (..))
import           Prelude                         hiding (Ordering (..), lex)
import           Test.Tasty
import           Test.Tasty.HUnit

parser :: TestTree
parser = testGroup "parser"
    [ testParse "SELECT name FROM users"
      ( Select ["name"] [ Sql "users" ] )
    , testParse "SELECT name, email FROM users"
        ( Select ["name", "email"] (map Sql (words "users" )))
    , testParse "SELECT name, email FROM users WHERE name = 'Daniel'"
        ( Select ["name", "email"] (map Sql (words "users WHERE name = 'Daniel'" )))
    , testParse "SELECT name, email FROM users WHERE name = 'Daniel' OR name = 'Bergey'"
      (Select ["name", "email" ] (map Sql (words "users WHERE name = 'Daniel' OR name = 'Bergey'" )))
    , testParse "SELECT name FROM users WHERE age = 35"
      (Select ["name"] (map Sql (words "users WHERE age = 35"  )))
    , testParse "SELECT name FROM users WHERE age = 35.5"
      ( Select ["name"] (map Sql (words "users WHERE age = 35.5"  )))
    , testParse "SELECT foo FROM bar WHERE baz > -2"
      ( Select ["foo"] (map Sql (words "bar WHERE baz > -2"  )))
    , testParse "SELECT foo FROM bar WHERE baz > -2e-2"
      ( Select ["foo"] (map Sql (words "bar WHERE baz > -2e-2"  )))
    , testParse "SELECT foo FROM bar WHERE baz = 2E-2"
      ( Select ["foo"] (map Sql (words "bar WHERE baz = 2E-2"  )))
    , testParse "SELECT foo FROM bar WHERE baz = 2E-2;"
      ( Select ["foo"] (map Sql (words "bar WHERE baz = 2E-2;"  )))
    ]

testParse :: TestName -> Select -> TestTree
testParse query expected = testCase query $
    assertEqual "" (Right expected) (parseSelect "<testcase>" query)
