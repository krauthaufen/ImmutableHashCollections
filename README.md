### Benchmarks

All tests are performed on `int` keys and values currently.  
Benchmarks are done using [BenchmarkDotNet](https://github.com/dotnet/BenchmarkDotNet)

* `HashMapOkasaki` is an implementation inspired by [Fast Mergeable Integer Maps](http://ittc.ku.edu/~andygill/papers/IntMap98.pdf) by *Chris Okasaki* and *Andrew Gill*
* `ImmutableDictionary` is the ImmutableDictionary from `System.Collections.Immutable`
* `FSharpX` uses `PersistentHashMap` from FSharpx.Collections
* `FSharpMap` is the standard search tree implementation from [FSharp.Core](https://github.com/dotnet/fsharp/blob/master/src/fsharp/FSharp.Core/map.fs) which is not really comparable but gives us a clue where the implementation stands
* `HAMT` is the V5 implementation from [HAMT.NET](https://github.com/alexandrnikitin/HAMT.NET)
* `ImTools` is the `ImHashMap` implementation from [ImTools](https://github.com/dadhi/ImTools)

### Results

Judging by the current benchmarks, the `HashMapOkasaki` implementation supports the fastest modification times and also shows lookup-times comparable to the fastest implementations. The only other alternative showing similar results is the `ImTools` implementation at the moment.

* [Lookup (existing)](https://github.com/krauthaufen/ImmutableHashCollections/wiki/Lookup---existing)
* [Lookup (nonexisting)](https://github.com/krauthaufen/ImmutableHashCollections/wiki/Lookup-nonexisting)
* [Add](https://github.com/krauthaufen/ImmutableHashCollections/wiki/Add)
* [Update](https://github.com/krauthaufen/ImmutableHashCollections/wiki/Update)
* [Remove](https://github.com/krauthaufen/ImmutableHashCollections/wiki/Remove)
* [OfArray](https://github.com/krauthaufen/ImmutableHashCollections/wiki/OfArray)
* [ToArray](https://github.com/krauthaufen/ImmutableHashCollections/wiki/ToArray)
