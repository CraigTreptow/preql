{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Preql.QuasiQuoter.Select.TH where

import           Preql.QuasiQuoter.Common
import           Preql.QuasiQuoter.Select.Parser (parseSelect)
import           Preql.QuasiQuoter.Select.Syntax as Syntax
import           Preql.Wire                      (Query (..))

import           Data.Foldable                   (foldl')
import           Data.String                     (IsString (..))
import           Data.Word                       (Word)
import           GHC.TypeNats
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax      (Lift (..))

import qualified Data.List.NonEmpty              as NE
import qualified Data.Text                       as T

-- | Convert a rewritten SQL string to a ByteString, leaving width free.
makeQuery :: Integer -> String -> Q Exp
makeQuery n string = [e|(fromString string :: Query $(nat)) |]
    where nat = pure (LitT (NumTyLit n))

-- | Given a SQL select query, @select@ counts the number of columns
-- selected, and tags the resulting 'Query'.  'query' will then verify
-- that the 'RowDecoder' expects the same number of columns.
-- Parameters are handled in the same way as in the 'sql'
-- quasiquoter
select :: QuasiQuoter
select = expressionOnly "select" $ \raw -> do
    loc <- location
    let e_ast = parseSelect (show loc) raw
    case e_ast of
        Left err -> error err
        Right parsed -> do
            let
                positionalCount = maxParam parsed
                (rewritten, haskellExpressions) = numberAntiquotes positionalCount parsed
                -- mkName, because we intend to capture what's in scope
                antiNames = map mkName haskellExpressions
                width = resultWidth parsed
            query <- makeQuery width rewritten
            queryParamsPair query positionalCount antiNames

maxParam :: Select -> Word
maxParam (Select _ rest) = foldl' nextParam 0 rest where
  nextParam maxParam = \case
      NumberedParam i -> max i maxParam
      _               -> maxParam

numberAntiquotes :: Word -> Select -> (String, [String])
numberAntiquotes mp select = (query, variableNames) where
  query = unwords ("SELECT" : NE.toList (targets select) ++ "FROM" : sqlStrings)
  (sqlStrings, variableNames) = go mp (from select)
  go _maxParam [] = ([], [])
  go maxParam (any : as) =
      case any of
          HaskellParam name -> let
              newParam = maxParam + 1
              (ss, ns) = go newParam as
              in (('$' : show newParam) : ss, name : ns)
          _ -> let (ss, ns) = go maxParam as in (showAny any : ss, ns)

resultWidth :: Select -> Integer
resultWidth (Select columns _) = toInteger (length columns)
