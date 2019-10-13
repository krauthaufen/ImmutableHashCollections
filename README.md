### Benchmarks

The test currently adds a single entry (`N/2`) to a (Hash)Map of size `N`.   
All tests are performed on `int` keys and values currently.  
Benchmarks are done using [BenchmarkDotNet](https://github.com/dotnet/BenchmarkDotNet)

* `HashMapOkasaki` is an implementation inspired by [Fast Mergeable Integer Maps](http://ittc.ku.edu/~andygill/papers/IntMap98.pdf) by *Chris Okasaki* and *Andrew Gill*
* `ImmutableDictionary` is the ImmutableDictionary from `System.Collections.Immutable` (using `SetItem`)
* `FSharpMap` is the standard search tree implementation from [FSharp.Core](https://github.com/dotnet/fsharp/blob/master/src/fsharp/FSharp.Core/map.fs) which is not really comparable but gives us a clue where the implementation stands

### Results



![Chart](https://fshade.org/images/chart2.svg)

```
BenchmarkDotNet=v0.11.5, OS=Windows 10.0.18362
Intel Core i7-8750H CPU 2.20GHz (Coffee Lake), 1 CPU, 12 logical and 6 physical cores
  [Host]     : .NET Framework 4.7.2 (CLR 4.0.30319.42000), 64bit LegacyJIT-v4.8.4018.0 DEBUG
  DefaultJob : .NET Framework 4.7.2 (CLR 4.0.30319.42000), 64bit RyuJIT-v4.8.4018.0


|                  Method |     N |      Mean |      Error |     StdDev |    Median |
|------------------------ |------ |----------:|-----------:|-----------:|----------:|
|      HashMapOkasaki_add |     0 |  26.51 ns |  0.0653 ns |  0.0545 ns |  26.50 ns |
|           FSharpMap_add |     0 |  15.17 ns |  0.0615 ns |  0.0514 ns |  15.18 ns |
| ImmutableDictionary_add |     0 | 103.60 ns |  0.2212 ns |  0.2070 ns | 103.63 ns |
|      HashMapOkasaki_add |    10 | 101.70 ns |  0.1482 ns |  0.1237 ns | 101.71 ns |
|           FSharpMap_add |    10 |  87.61 ns |  0.1537 ns |  0.1283 ns |  87.63 ns |
| ImmutableDictionary_add |    10 | 336.04 ns |  0.5483 ns |  0.5129 ns | 336.10 ns |
|      HashMapOkasaki_add |    20 | 114.53 ns |  1.5303 ns |  1.3566 ns | 114.56 ns |
|           FSharpMap_add |    20 |  98.06 ns |  0.5223 ns |  0.4362 ns |  97.93 ns |
| ImmutableDictionary_add |    20 | 333.90 ns |  1.5497 ns |  1.3738 ns | 334.22 ns |
|      HashMapOkasaki_add |    30 | 115.09 ns |  0.1421 ns |  0.1260 ns | 115.07 ns |
|           FSharpMap_add |    30 | 161.72 ns |  0.2308 ns |  0.2046 ns | 161.68 ns |
| ImmutableDictionary_add |    30 | 402.61 ns |  2.4298 ns |  2.0290 ns | 402.51 ns |
|      HashMapOkasaki_add |    40 | 125.17 ns |  0.8483 ns |  0.7935 ns | 124.81 ns |
|           FSharpMap_add |    40 |  98.09 ns |  0.2323 ns |  0.2059 ns |  98.11 ns |
| ImmutableDictionary_add |    40 | 345.83 ns |  0.9545 ns |  0.7971 ns | 346.05 ns |
|      HashMapOkasaki_add |    50 | 126.12 ns |  0.3018 ns |  0.2823 ns | 126.12 ns |
|           FSharpMap_add |    50 | 200.95 ns |  0.9305 ns |  0.8248 ns | 200.97 ns |
| ImmutableDictionary_add |    50 | 470.43 ns |  2.8329 ns |  2.5113 ns | 470.05 ns |
|      HashMapOkasaki_add |   100 | 135.89 ns |  0.1650 ns |  0.1288 ns | 135.92 ns |
|           FSharpMap_add |   100 | 242.51 ns |  9.6215 ns | 27.9137 ns | 232.58 ns |
| ImmutableDictionary_add |   100 | 492.13 ns |  9.7731 ns | 21.6566 ns | 484.70 ns |
|      HashMapOkasaki_add |   200 | 156.56 ns |  3.0464 ns |  2.8496 ns | 155.77 ns |
|           FSharpMap_add |   200 | 223.79 ns |  4.6180 ns | 11.6703 ns | 218.90 ns |
| ImmutableDictionary_add |   200 | 473.70 ns |  4.7068 ns |  4.4028 ns | 473.34 ns |
|      HashMapOkasaki_add |   300 | 162.29 ns |  0.8249 ns |  0.7313 ns | 162.04 ns |
|           FSharpMap_add |   300 | 259.65 ns |  3.0971 ns |  2.8971 ns | 259.97 ns |
| ImmutableDictionary_add |   300 | 537.48 ns |  2.5646 ns |  2.3989 ns | 537.48 ns |
|      HashMapOkasaki_add |   400 | 162.31 ns |  1.5211 ns |  1.4228 ns | 162.10 ns |
|           FSharpMap_add |   400 | 213.27 ns |  2.1327 ns |  1.9949 ns | 213.17 ns |
| ImmutableDictionary_add |   400 | 475.54 ns |  4.4977 ns |  4.2071 ns | 475.58 ns |
|      HashMapOkasaki_add |   500 | 161.37 ns |  1.0288 ns |  0.9624 ns | 161.52 ns |
|           FSharpMap_add |   500 | 288.98 ns |  2.0322 ns |  1.9009 ns | 289.12 ns |
| ImmutableDictionary_add |   500 | 622.17 ns |  4.6332 ns |  4.3339 ns | 622.44 ns |
|      HashMapOkasaki_add |  1000 | 175.75 ns |  1.1037 ns |  1.0324 ns | 175.91 ns |
|           FSharpMap_add |  1000 | 289.70 ns |  2.0212 ns |  1.8906 ns | 290.32 ns |
| ImmutableDictionary_add |  1000 | 613.76 ns | 12.1243 ns | 13.9623 ns | 607.13 ns |
|      HashMapOkasaki_add |  2000 | 192.98 ns |  2.3712 ns |  2.2180 ns | 192.51 ns |
|           FSharpMap_add |  2000 | 314.98 ns |  6.3624 ns | 15.4868 ns | 314.31 ns |
| ImmutableDictionary_add |  2000 | 644.28 ns | 12.8488 ns | 28.4721 ns | 629.88 ns |
|      HashMapOkasaki_add |  3000 | 204.80 ns |  0.8219 ns |  0.7688 ns | 204.81 ns |
|           FSharpMap_add |  3000 | 368.84 ns |  2.8502 ns |  2.5266 ns | 368.59 ns |
| ImmutableDictionary_add |  3000 | 733.37 ns |  1.9589 ns |  1.7365 ns | 733.28 ns |
|      HashMapOkasaki_add |  4000 | 202.83 ns |  0.9091 ns |  0.8059 ns | 202.76 ns |
|           FSharpMap_add |  4000 | 286.84 ns |  1.4937 ns |  1.3972 ns | 286.97 ns |
| ImmutableDictionary_add |  4000 | 598.66 ns |  1.8545 ns |  1.5486 ns | 598.90 ns |
|      HashMapOkasaki_add |  5000 | 214.64 ns |  0.8494 ns |  0.7946 ns | 214.71 ns |
|           FSharpMap_add |  5000 | 373.76 ns |  1.3844 ns |  1.2273 ns | 374.15 ns |
| ImmutableDictionary_add |  5000 | 739.44 ns |  2.6327 ns |  2.4626 ns | 738.80 ns |
|      HashMapOkasaki_add | 10000 | 226.79 ns |  0.5501 ns |  0.5146 ns | 226.63 ns |
|           FSharpMap_add | 10000 | 373.25 ns |  1.0569 ns |  0.9370 ns | 373.15 ns |
| ImmutableDictionary_add | 10000 | 739.59 ns |  3.5190 ns |  3.2917 ns | 739.26 ns |
|      HashMapOkasaki_add | 20000 | 237.70 ns |  0.7309 ns |  0.6837 ns | 237.64 ns |
|           FSharpMap_add | 20000 | 372.91 ns |  1.4016 ns |  1.1704 ns | 372.70 ns |
| ImmutableDictionary_add | 20000 | 748.31 ns |  1.6950 ns |  1.5855 ns | 748.15 ns |
|      HashMapOkasaki_add | 30000 | 239.02 ns |  1.1385 ns |  1.0649 ns | 238.84 ns |
|           FSharpMap_add | 30000 | 436.69 ns |  1.5793 ns |  1.4773 ns | 436.08 ns |
| ImmutableDictionary_add | 30000 | 883.84 ns |  2.2076 ns |  2.0650 ns | 883.81 ns |
```


