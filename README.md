# PreQL

Before you Post(gres)QL, **preql**.

A Haskell SQL library.

1. [Quickstart](#quickstart)
2. [Vision](#vision-parsing-sql-in-haskell-quasiquotes)

## Current Status

**preql** provides a low-level interface to PostgreSQL and a quasiquoter that converts
inline variable names to SQL parameters.  Higher-level interfaces, checking SQL syntax &
schema, are planned.

## Quickstart

```haskell
    import Preql -- The `Preql` module re-exports the interface useful to typical applications.
    import Control.Monad.Trans.Reader (runReaderT)

    main :: IO ()
    main = do
        conn <- connectdb "" -- Get a database connection with default connection string

        -- You can write a SQL instance to replace this ReaderT with
        -- your own application state, logging, error handling
        flip runReaderT conn $
            -- A simple query with no parameters
            cats <- query [sql| SELECT name, age FROM cats |]
            for_ cats \(cat :: (Text, Int)) -> print cat

            -- A query with parameters
            let minAge = 10
            oldCats <- query [sql| SELECT name, age FROM cats where age > ${minAge}|]
            for_ oldCats \(cat :: (Text, Int)) -> print cat

            -- A query that doesn't return rows
            query_ [sql| UPDATE cats SET age = 0 where age < 1 |]

            -- Two queries in a transaction
            moreOldCats <- runTransaction $ do
                maxAge <- V.head <$> query [sql| SELECT max(age) FROM cats |]
                query [sql| SELECT name FROM cats WHERE age = ${maxAge} |]
                -- Just an example; you could make this one DB roundtrip
            traverse_ putStrLn moreOldCats
```

## Vision: Parsing SQL in Haskell Quasiquotes

Can GHC check that a query is sytactically correct at compile time, without having
Postgres run it?

I want to
- write standard (Postgres) SQL
- support all of the Postgres features that I depend on in a single library
- catch SQL syntax errors during Haskell type checking
- catch SQL - Haskell type mismatches during Haskell type checking

To hedge my bets between the goal of supporting Postgres's extensive feature set &
providing meaningful type errors, I plan to implement 3 quasiquoters:

- parameter (and result column?) antiquotes, no attempt at interpreting the SQL
- SQL syntax checking, without requiring a schema
- with a schema provided, check that queries, parameters, and results are type-correct

I see several reasons to support literal SQL syntax:
- many people already know it; learning it will be useful for a long time
- non-Haskell developers on the project are also likely to know it
- efficient interactive development of the SQL query, before parameterizing & inserting in Haskell code
- easy adoption from existing code bases using postgresql-simple or hasql (or other SQL-string libraries)

### Prior Art

Haskell libraries that I've used in production seem to fall into 3 categories:

- SQL as strings, with syntax & types only checked when the query runs
- simple non-SQL interfaces that are convenient for CRUD, but don't cover joins, aggregates, and other SQL features
- complex EDSLs that cover much of SQL, with a new syntax & lots of fancy type hackery

Each approach addresses some of the goals under **Vision**, but none lets me both write
literal SQL & provides a reasonable degree of type checking.

The most similar design that I've found in Haskell is
[hasql-th](http://hackage.haskell.org/package/hasql-th).

[postgresql-typed](https://hackage.haskell.org/package/postgresql-typed) also occupies a
similar point in the design space.  It provides a quasiquoter, and connects to Postgres at
compile time to validate the queries.  I haven't tried it out yet, but it seems like a
very promising approach.

If you know of other libraries with similar ambitions, please let me know.

