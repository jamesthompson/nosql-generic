# NoSQL Generic

Haskell generic (de)serialization typeclass derivation of popular Cloud NoSQL database type conversion functions for record types, making use of **GHC.Generics**.

It seemed like a potentially useful idea given how complicated the cloud service NoSQL database datatypes are, and how much time is spent in SOAs marshalling data between various different representations for networking and storage.

Presently targeting and tested on **GHC 7.10.3**.

- Supports: [Gogol Datastore](https://hackage.haskell.org/package/gogol-datastore) generic record-type Datastore entity serialization and deserialization.
  - That is, it automatically generates functions to convert between types `a -> (Maybe Entity)` and backwards from `Entity -> Maybe a`, where there is an instance of `Generic` in scope.
  - It makes heavy use of [lens](https://hackage.haskell.org/package/lens) as the Gogol library depends on it for all data work.
- Future: [Amazonka DynamoDB](https://hackage.haskell.org/package/amazonka-dynamodb).
