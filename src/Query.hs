{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |

module Query where

import           FromSql
import           Connection

import           Control.Concurrent.MVar
import           Control.Monad
import           Data.ByteString (ByteString)
import           Data.String (IsString)

import qualified Database.PostgreSQL.LibPQ as PQ

-- | A @Query@ is a string ready to be passed to Postgres, with
-- phantom type parameters describing its parameters and result.
-- Depending how the @Query@ was constructed, these parameters may be
-- inferred from context (offering no added type safety), or be
-- partly synthesized from the underlying string.
--
-- The IsString instance does no validation; the limited instances
-- discourage directly manipulating strings, with the high risk of SQL
-- injection.
newtype Query params result = Query ByteString
    deriving (Show, IsString)

data QueryAndParams params result = Q (Query params result) params

-- TODO handle params
-- TODO handle multiple columns
runQueryWith :: SqlDecoder r -> Connection -> Query () r -> IO (Either String r)
runQueryWith dec conn (Query query) =
    withMVar (connectionHandle conn) $ \connRaw -> do
        Just result <- PQ.exec connRaw query
        ok <- checkTypes dec result
        if ok
            -- then (parse dec <=< emptyValue) <$> PQ.getvalue result (PQ.Row 0) (PQ.Col 0)
            then runDecoder dec result (PQ.Row 0)
            else return (Left "Types do not match")

runQuery :: FromSql r => Connection -> Query () r -> IO (Either String r)
runQuery = runQueryWith fromSql
