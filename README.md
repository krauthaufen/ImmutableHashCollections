### Benchmarks

All tests are performed on `int` keys and values currently.  
Benchmarks are done using [BenchmarkDotNet](https://github.com/dotnet/BenchmarkDotNet)

* `HashMapOkasaki` is an implementation inspired by [Fast Mergeable Integer Maps](http://ittc.ku.edu/~andygill/papers/IntMap98.pdf) by *Chris Okasaki* and *Andrew Gill*
* `ImmutableDictionary` is the ImmutableDictionary from `System.Collections.Immutable` (using `SetItem`)
* `FSharpX` uses `PersistentHashMap` from FSharpx.Collections
* `FSharpMap` is the standard search tree implementation from [FSharp.Core](https://github.com/dotnet/fsharp/blob/master/src/fsharp/FSharp.Core/map.fs) which is not really comparable but gives us a clue where the implementation stands


### Results

* [Lookup (existing)](https://github.com/krauthaufen/ImmutableHashCollections/wiki/Lookup---existing)
* [Lookup (nonexisting)](https://github.com/krauthaufen/ImmutableHashCollections/wiki/Lookup-nonexisting)
