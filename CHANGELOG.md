# 0.3 (2020-06-18)

- lookup types by name when OID is not known statically
- specify isolation level when running a transaction
- stop trying to fake nested transactions
- provide direct access to a Connection when that control is needed

# 0.2 (2020-03-31)

- now we have documentation
- rearrange some code, re-export modules to support docs

# 0.1 (2020-02-29)

0.1 highlights include:

- binary wire format
- check that Postgres sent expected type before decoding
- Quasiquoter supports mixing numbered & antiquoted params
