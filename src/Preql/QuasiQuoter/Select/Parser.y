{
{-# LANGUAGE DuplicateRecordFields #-}
module Preql.QuasiQuoter.Select.Parser where

import Preql.QuasiQuoter.Select.Syntax
import Preql.QuasiQuoter.Select.Lex (Alex, LocToken(..), Token)

import           Prelude hiding (LT, GT, lex)
import           Data.List.NonEmpty        (NonEmpty (..))

import qualified Preql.QuasiQuoter.Select.Lex as L
import qualified Data.List.NonEmpty as NE
}

%name parseSelect_ Select
%tokentype { L.LocToken }
%monad { Alex }
%lexer { lexwrap } {  L.LocToken _ L.EOF }
%error { happyError }

%token
    SELECT { LocToken _ L.SELECT }
    FROM { LocToken _ L.FROM }
    COMMA { LocToken _ L.COMMA }
    IDENT { LocToken _ (L.Ident $$) }

    PARAM { LocToken _ (L.NumberedParam $$) }
    HASKELL_PARAM { LocToken _ (L.HaskellParam $$) }

    SQL { LocToken _ (L.Sql $$) }
%%

-- This actually only tries to parse what's between SELECT & FROM, and accepts anything after that.
Select
    : SELECT target_list FROM any_list { Select $2 (reverse $4) }
    
-- These lists are non-empty by construction, but not by type. List head is the right-most element.

list(el)
    : el { [$1] }
    | list(el) COMMA el { $3 : $1 }

target_list : list(target_el) { NE.fromList (reverse $1) }

target_el :: { String }
    : IDENT { $1 }
    -- : a_expr AS ColLabel { ColumnTarget (ColumnRef $1 (Just $3)) }
    -- | a_expr Name { ColumnTarget (ColumnRef $1 (Just $2)) }
    -- | a_expr { ColumnTarget (ColumnRef $1 Nothing) }
    -- | '*' { Star }

any_list :: { [Any ] }
    : any { [$1] }
    | any_list any { $2 : $1 }

-- accept anything; rewrap the lexer tokens
any :: { Any }
    : SELECT { Sql "SELECT" }
    | FROM { Sql "FROM" }
    | COMMA { Sql "," }
    | IDENT { Sql $1 }
    | PARAM { NumberedParam $1 }
    | HASKELL_PARAM { HaskellParam $1 }
    | SQL { Sql $1 }

{

-- from https://github.com/dagit/happy-plus-alex/blob/master/src/Parser.y

lexwrap :: (L.LocToken -> Alex a) -> Alex a
lexwrap = (L.alexMonadScan' >>=)

happyError :: L.LocToken -> Alex a
happyError (L.LocToken p t) =
  L.alexError' p ("parse error at token '" ++ L.unLex t ++ "'")

parseSelect :: FilePath -> String -> Either String Select
parseSelect = L.runAlex' parseSelect_

}