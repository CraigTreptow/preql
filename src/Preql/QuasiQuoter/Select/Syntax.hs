-- |

module Preql.QuasiQuoter.Select.Syntax where

import           Data.List.NonEmpty (NonEmpty (..))

data Select = Select
    { targets :: NonEmpty String
    , from    :: [Any]
    }
    deriving (Show, Eq)

data Any = NumberedParam Word
    | HaskellParam String
    | Sql String
    deriving (Show, Eq)

showAny :: Any -> String
showAny (NumberedParam n)   = '$' : show n
showAny (HaskellParam name) = "${" ++ name ++ "}"
showAny (Sql s)             = s
