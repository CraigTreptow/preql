module Preql.QuasiQuoter.Common where

import           Data.Word                  (Word)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax (Lift (..))

-- | A list of n Names beginning with the given character
cNames :: Char -> Int -> Q [Name]
cNames c n = traverse newName (replicate n (c : ""))

tupleOrSingle :: [Name] -> Exp
tupleOrSingle names = case names of
    [name] -> VarE name
    vs     -> TupE $ map VarE vs

expressionOnly :: String -> (String -> Q Exp) -> QuasiQuoter
expressionOnly name qq = QuasiQuoter
    { quoteExp = qq
    , quotePat = \_ -> error $ "qq " ++ name ++ " cannot be used in pattern context"
    , quoteType = \_ -> error $ "qq " ++ name ++ " cannot be used in type context"
    , quoteDec = \_ -> error $ "qq " ++ name ++ " cannot be used in declaration context"
    }

-- TODO TExp for query?
-- | Takes the Exp of a Query, the number of positional params in the
-- query, and the names of named params.
queryParamsPair :: Exp -> Word -> [Name] -> Q Exp
queryParamsPair  query positionalCount antiNames =
    case positionalCount of
        0 -> -- only antiquotes (or no params)
            return $ TupE [query, tupleOrSingle antiNames]
        1 -> do -- one positional param, doesn't take a tuple
            patternName <- newName "c"
            return $ LamE [VarP patternName]
                (TupE [query, tupleOrSingle (patternName : antiNames)])
        _ -> do -- at least two positional parameters
            patternNames <- cNames 'q' (fromIntegral positionalCount)
            return $ LamE
                [TupP (map VarP patternNames)]
                (TupE [query, tupleOrSingle (patternNames ++ antiNames)])
