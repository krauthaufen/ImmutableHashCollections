### Benchmarks

The test currently adds a single entry (in the middle) to a (Hash)Map of size `N`.   
All tests are performed on `int` keys and values currently.  
Benchmarks are done using [BenchmarkDotNet](https://github.com/dotnet/BenchmarkDotNet)

* `HashMapOkasaki` is an implementation inspired by [Fast Mergeable Integer Maps](http://ittc.ku.edu/~andygill/papers/IntMap98.pdf) by *Chris Okasaki* and *Andrew Gill*
* `FSharpMap` is the standard search tree implementation from [FSharp.Core](https://github.com/dotnet/fsharp/blob/master/src/fsharp/FSharp.Core/map.fs) which is not really comparable but gives us a clue where the implementation stands

### Results



![Chart](https://fshade.org/images/chart.svg)

```
// * Summary *

BenchmarkDotNet=v0.11.5, OS=Windows 10.0.18362
Intel Core i7-8750H CPU 2.20GHz (Coffee Lake), 1 CPU, 12 logical and 6 physical cores
  [Host]     : .NET Framework 4.7.2 (CLR 4.0.30319.42000), 64bit LegacyJIT-v4.8.4018.0 DEBUG
  DefaultJob : .NET Framework 4.7.2 (CLR 4.0.30319.42000), 64bit RyuJIT-v4.8.4018.0


|             Method |    N |      Mean |     Error |    StdDev |
|------------------- |----- |----------:|----------:|----------:|
| HashMapOkasaki_add |    0 |  30.34 ns | 0.6798 ns |  1.154 ns |
|      FSharpMap_add |    0 |  17.12 ns | 0.4461 ns |  1.243 ns |
| HashMapOkasaki_add |  100 | 159.88 ns | 2.9785 ns |  2.640 ns |
|      FSharpMap_add |  100 | 240.44 ns | 3.7425 ns |  3.501 ns |
| HashMapOkasaki_add |  200 | 170.43 ns | 3.7309 ns | 10.883 ns |
|      FSharpMap_add |  200 | 241.85 ns | 4.8574 ns | 12.965 ns |
| HashMapOkasaki_add |  300 | 193.01 ns | 3.8517 ns |  3.603 ns |
|      FSharpMap_add |  300 | 292.95 ns | 5.9147 ns | 11.948 ns |
| HashMapOkasaki_add |  400 | 186.83 ns | 3.7194 ns |  6.214 ns |
|      FSharpMap_add |  400 | 235.73 ns | 4.7531 ns |  6.506 ns |
| HashMapOkasaki_add |  500 | 190.74 ns | 3.6186 ns |  3.208 ns |
|      FSharpMap_add |  500 | 322.11 ns | 6.4991 ns |  5.761 ns |
| HashMapOkasaki_add | 1000 | 199.41 ns | 4.0022 ns |  6.796 ns |
|      FSharpMap_add | 1000 | 318.06 ns | 6.3736 ns | 12.280 ns |
| HashMapOkasaki_add | 2000 | 218.15 ns | 4.3799 ns | 10.988 ns |
|      FSharpMap_add | 2000 | 327.49 ns | 7.9089 ns | 22.307 ns |
| HashMapOkasaki_add | 3000 | 236.15 ns | 4.7220 ns |  6.772 ns |
|      FSharpMap_add | 3000 | 411.21 ns | 8.2644 ns | 11.852 ns |
| HashMapOkasaki_add | 4000 | 234.08 ns | 4.6894 ns |  8.575 ns |
|      FSharpMap_add | 4000 | 321.02 ns | 6.4171 ns | 12.053 ns |
| HashMapOkasaki_add | 5000 | 246.37 ns | 3.9552 ns |  3.700 ns |
|      FSharpMap_add | 5000 | 387.73 ns | 4.4223 ns |  3.453 ns |
```


