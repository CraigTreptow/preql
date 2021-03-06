-- |
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Url: https://github.com/phadej/postgresql-simple/tree/master/src/Database/PostgreSQL/Simple/TypeInfo
------------------------------------------------------------------------------

module Preql.Wire.TypeInfo.Types where

import Data.ByteString(ByteString)
import Database.PostgreSQL.LibPQ(Oid)
import Data.Vector(Vector)

-- | A structure representing some of the metadata regarding a PostgreSQL
--   type,  mostly taken from the @pg_type@ table.

data TypeInfo

  = Basic { typoid      :: {-# UNPACK #-} !Oid
          , typcategory :: {-# UNPACK #-} !Char
          , typdelim    :: {-# UNPACK #-} !Char
          , typname     :: !ByteString
          }

  | Array { typoid      :: {-# UNPACK #-} !Oid
          , typcategory :: {-# UNPACK #-} !Char
          , typdelim    :: {-# UNPACK #-} !Char
          , typname     :: !ByteString
          , typelem     :: !TypeInfo
          }

  | Range { typoid      :: {-# UNPACK #-} !Oid
          , typcategory :: {-# UNPACK #-} !Char
          , typdelim    :: {-# UNPACK #-} !Char
          , typname     :: !ByteString
          , rngsubtype  :: !TypeInfo
          }

  | Composite { typoid      :: {-# UNPACK #-} !Oid
              , typcategory :: {-# UNPACK #-} !Char
              , typdelim    :: {-# UNPACK #-} !Char
              , typname     :: !ByteString
              , typrelid    :: {-# UNPACK #-} !Oid
              , attributes  :: !(Vector Attribute)
              }

    deriving (Show)

data Attribute
   = Attribute { attname :: !ByteString
               , atttype :: !TypeInfo
               }
     deriving (Show)
