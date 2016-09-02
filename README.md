# NoSQL Generic

Haskell generic (de)serialization typeclass derivation of popular Cloud NoSQL database type conversion functions for record types, making use of **GHC.Generics**.

Presently targeting and tested on GHC 7.10.3.

- Supports: [Gogol Datastore](https://hackage.haskell.org/package/gogol-datastore) generic record-type Datastore entity serialization and deserialization. That is, it automatically generates functions to convert between types `a -> (Maybe Entity)` and backwards from `Entity -> Maybe a`, where there is an instance of `Generic` in scope.
- Future: [Amazonka DynamoDB](https://hackage.haskell.org/package/amazonka-dynamodb).
