{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Test.Syntax.Parser where

import Preql.QuasiQuoter.Syntax.Name
import Preql.QuasiQuoter.Syntax.Parser
import Preql.QuasiQuoter.Syntax.Syntax
import qualified Preql.QuasiQuoter.Syntax.Lex as L

import Data.List.NonEmpty (NonEmpty(..))
import Prelude hiding (Ordering(..), lex)
import Test.Tasty
import Test.Tasty.HUnit

parser :: TestTree
parser = testGroup "parser"
    [ testParse "DELETE FROM taffy"
        (QD (Delete (mkName "taffy") Nothing))
    , testParse "dEleTe FROM taffy WHERE flavor = 'blueberry'"
      (QD Delete
          { table = mkName "taffy"
          , conditions = Just (Compare Eq (mkName "flavor") (Lit (T"blueberry")))
          })
    , testParse "DELETE FROM users WHERE email != 'bergey@teallabs.org'"
      (QD Delete
       { table = mkName "users"
       , conditions = Just (Compare NEq (mkName "email") (Lit (T "bergey@teallabs.org")))
       })
    , testParse "INSERT INTO users (email) VALUES ('bergey@teallabs.org')"
        (QI Insert
            { table = mkName "users"
            , columns = mkName "email" :| []
            , values = Lit (T "bergey@teallabs.org") :| []
            })
    , testParse "INSERT INTO addresses (street, country) VALUES ('4 Ames St', 'USA')"
      (QI Insert
       { table = "addresses"
       , columns = "street" :| ["country" ]
       , values = Lit (T "4 Ames St") :| [ Lit (T "USA") ]
       })
    , testParse "SELECT name FROM users"
      (QS OldSelect
       { table = "users"
       , columns = CRef (ColumnRef (Var "name") Nothing) :| []
       , conditions = Nothing
       })
    , testParse "SELECT name, email FROM users"
      (QS OldSelect
       { table = "users"
       , columns = CRef (ColumnRef (Var "name") Nothing) :| [CRef (ColumnRef (Var "email") Nothing)]
       , conditions = Nothing
       })
    , testParse "SELECT name, email FROM users WHERE name = 'Daniel'"
      (QS OldSelect
       { table = "users"
       , columns = CRef (ColumnRef (Var "name") Nothing) :| [CRef (ColumnRef (Var "email") Nothing)]
       , conditions = Just (Compare Eq "name" (Lit (T "Daniel")))
       })
    , testParse "SELECT name, email FROM users WHERE name = 'Daniel' OR name = 'Bergey'"
      (QS OldSelect
       { table = "users"
       , columns = CRef (ColumnRef (Var "name") Nothing) :| [CRef (ColumnRef (Var "email") Nothing)]
       , conditions = Just (Or (Compare Eq "name" (Lit (T "Daniel"))) (Compare Eq "name" (Lit (T "Bergey"))))
       })
    , testParse "SELECT name FROM users WHERE age = 35"
        -- We currently parse integers & decimals all to Double
        -- Just test that both parser rules work
      (QS OldSelect
       { table = "users"
       , columns = CRef (ColumnRef (Var "name") Nothing) :| []
       , conditions = Just (Compare Eq "age" (Lit (F 35)))
       })
    , testParse "SELECT name FROM users WHERE age = 35.5"
      (QS OldSelect
       { table = "users"
       , columns = CRef (ColumnRef (Var "name") Nothing) :| []
       , conditions = Just (Compare Eq "age" (Lit (F 35.5)))
       })
    , testParse "SELECT foo FROM bar WHERE baz > -2"
      (QS OldSelect
          { table = "bar"
          , columns = CRef (ColumnRef (Var "foo") Nothing) :| []
          , conditions = Just (Compare GT "baz" (Lit (F (-2) )))
          })
    , testParse "SELECT foo FROM bar WHERE baz = 2e-2"
        (QS OldSelect
          { table = "bar"
          , columns = CRef (ColumnRef (Var "foo") Nothing) :| []
          , conditions = Just (Compare Eq "baz" (Lit (F 0.02)))
          })
    , testParse "SELECT foo FROM bar WHERE baz = 2E-2"
        (QS OldSelect
          { table = "bar"
          , columns = CRef (ColumnRef (Var "foo") Nothing) :| []
          , conditions = Just (Compare Eq "baz" (Lit (F 0.02)))
          })
    , testParseExpr "2 * 3 + 1"
      (BinOp Add (BinOp Mul (Lit (F 2)) (Lit (F 3))) (Lit (F 1)))
    , testParseExpr "1 + 2 * 3"
      (BinOp Add (Lit (F 1)) (BinOp Mul (Lit (F 2)) (Lit (F 3))) )
    , testCase "lex '' escape" $
      assertEqual "" (Right [L.String "foo'bar", L.EOF]) (L.testLex "'foo''bar'")
    , testCase "lex semicolon" $
      assertEqual "" (Right [L.SELECT, L.Number 2.0, L.Add, L.Number 3.0, L.Semicolon, L.EOF]) (L.testLex "SELECT 2 + 3;")
    , testParse "SELECT foo FROM bar WHERE baz = 2E-2;"
      (QS OldSelect
          { table = "bar"
          , columns = CRef (ColumnRef (Var "foo") Nothing) :| []
          , conditions = Just (Compare Eq "baz" (Lit (F 0.02)))
          })
    ]

testParse :: TestName -> Query -> TestTree
testParse query expected = testCase query $
    assertEqual "" (Right expected) (parseQuery "<testcase>" query)

testParseExpr :: TestName -> Expr -> TestTree
testParseExpr query expected = testCase query $
    assertEqual "" (Right expected) (parseExpr "<testcase>" query)