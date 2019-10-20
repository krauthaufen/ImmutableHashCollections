namespace ImmutableHashCollections.Tests

open ImmutableHashCollections
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open System.Collections.Immutable
open BenchmarkDotNet.Reports
open FSharpx.Collections

module Constants =
    [<Literal>]
    #if SMALL
    let maxIter = 70
    #else
    let maxIter = 100
    #endif

type Instance =
    {
        okasaki     : HashMapOkasaki<int, int>
        fsharp      : Map<int, int>
        system      : ImmutableDictionary<int, int>
        fsharpx     : PersistentHashMap<int, int>
        hamt        : HAMT.NET.V5.ImmutableDictionary<int, int>
        imtools     : ImTools.ImHashMap<int, int>
        existing    : int[]
        nonExisting : int[]
    }

module Instance = 
    let create (n : int) =


        let rand = System.Random()

        let numbers = 
            let mutable cnt = 0
            let mutable taken = Set.empty
            while cnt < n do
                let n = rand.Next()
                if not (Set.contains n taken) then 
                    taken <- Set.add n taken
                    cnt <- cnt + 1
            taken
            
        let nonExisting =
            let mutable cnt = 0
            let mutable taken = Set.empty
            while cnt < n do
                let n = rand.Next()
                if not (Set.contains n taken) && not (Set.contains n numbers) then 
                    taken <- Set.add n taken
                    cnt <- cnt + 1
            taken
            

        let list =
            numbers |> Set.toList |> List.map (fun i ->
                i, i
            )
        {
            okasaki = HashMapOkasaki.ofList list
            fsharp = Map.ofList list
            fsharpx = PersistentHashMap.ofSeq list
            system =
                (ImmutableDictionary.Empty, list) ||> List.fold (fun d (k,v) ->
                    d.SetItem(k, v)
                )
            hamt = HAMT.NET.V5.ImmutableDictionary.Empty
                //(HAMT.NET.V5.ImmutableDictionary.Empty, list) ||> List.fold (fun d (k,v) ->
                //    d.Add(k, v)
                //)
            imtools = 
                (ImTools.ImHashMap<int, int>.Empty, list) ||> List.fold (fun d (k,v) ->
                    d.AddOrUpdate(k, v)
                )

            nonExisting = Set.toArray nonExisting
            existing = Set.toArray numbers
        }

[<PlainExporter; MemoryDiagnoser; MaxIterationCount(Constants.maxIter)>]
type UpdateBenchmark() =
    #if SMALL
    [<DefaultValue; Params(10000)>]
    #else
    [<DefaultValue; Params(10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000)>]
    #endif
    val mutable public N : int

    let mutable instance = Instance.create 0

    [<GlobalSetup>]
    member x.Setup() =
        instance <- Instance.create x.N


    [<Benchmark>]
    member x.HashMapOkasaki_update() =
        let mutable h = instance.okasaki
        for key in instance.existing do
            h <- HashMapOkasaki.add key -123 h
        
    [<Benchmark>]
    member x.ImTools_update() =
        let mutable h = instance.imtools
        for key in instance.existing do
            h <- h.AddOrUpdate(key, -123)
        
    [<Benchmark>]
    member x.FSharpMap_update() =
        let mutable h = instance.fsharp
        for key in instance.existing do
            h <- Map.add key -123 h
        
    [<Benchmark>]
    member x.HAMT_update() =
        let mutable h = instance.hamt
        for key in instance.existing do
            h <- h.Add(key, -123)
        
    [<Benchmark>]
    member x.FSharpX_update() =
        let mutable h = instance.fsharpx
        for key in instance.existing do
            h <- PersistentHashMap.add key -123 h
        
    [<Benchmark>]
    member x.ImmutableDictionary_update() =
        let mutable h = instance.system
        for key in instance.existing do
            h <- h.SetItem(key, -123)

[<PlainExporter; MemoryDiagnoser; MaxIterationCount(Constants.maxIter)>]
type AddBenchmark() =
    #if SMALL
    [<DefaultValue; Params(10000)>]
    #else
    [<DefaultValue; Params(10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000)>]
    #endif
    val mutable public N : int

    let mutable instance = Instance.create 0

    [<GlobalSetup>]
    member x.Setup() =
        instance <- Instance.create x.N


    [<Benchmark>]
    member x.HashMapOkasaki_add() =
        let mutable h = instance.okasaki
        for key in instance.nonExisting do
            h <- HashMapOkasaki.add key -123 h
        
    [<Benchmark>]
    member x.ImTools_add() =
        let mutable h = instance.imtools
        for key in instance.nonExisting do
            h <- h.AddOrUpdate(key, -123)

    [<Benchmark>]
    member x.FSharpMap_add() =
        let mutable h = instance.fsharp
        for key in instance.nonExisting do
            h <- Map.add key -123 h
        
    [<Benchmark>]
    member x.HAMT_add() =
        let mutable h = instance.hamt
        for key in instance.nonExisting do
            h <- h.Add(key, -123)
        
    [<Benchmark>]
    member x.FSharpX_add() =
        let mutable h = instance.fsharpx
        for key in instance.nonExisting do
            h <- PersistentHashMap.add key -123 h
        
    [<Benchmark>]
    member x.ImmutableDictionary_add() =
        let mutable h = instance.system
        for key in instance.nonExisting do
            h <- h.SetItem(key, -123)

[<PlainExporter; MemoryDiagnoser; MaxIterationCount(Constants.maxIter)>]
type RemoveBenchmark() =
    #if SMALL
    [<DefaultValue; Params(10000)>]
    #else
    [<DefaultValue; Params(10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000)>]
    #endif
    val mutable public N : int

    let mutable instance = Instance.create 0

    [<GlobalSetup>]
    member x.Setup() =
        instance <- Instance.create x.N


    [<Benchmark>]
    member x.HashMapOkasaki_remove() =
        let mutable h = instance.okasaki
        for key in instance.existing do
            h <- HashMapOkasaki.remove key h
        
    [<Benchmark>]
    member x.ImTools_remove() =
        let mutable h = instance.imtools
        for key in instance.existing do
            h <- h.Remove(key)

        
    [<Benchmark>]
    member x.FSharpMap_remove() =
        let mutable h = instance.fsharp
        for key in instance.existing do
            h <- Map.remove key h
        
    //[<Benchmark>]
    //member x.HAMT_update() =
    //    let mutable h = instance.hamt
    //    for key in instance.existing do
    //        h <- h.Remove key
        
    [<Benchmark>]
    member x.FSharpX_remove() =
        let mutable h = instance.fsharpx
        for key in instance.existing do
            h <- PersistentHashMap.remove key h
        
    [<Benchmark>]
    member x.ImmutableDictionary_remove() =
        let mutable h = instance.system
        for key in instance.existing do
            h <- h.Remove key

[<PlainExporter; MemoryDiagnoser; MaxIterationCount(Constants.maxIter)>]
type PositiveLookupBenchmark() =
    #if SMALL
    [<DefaultValue; Params(10000)>]
    #else
    [<DefaultValue; Params(10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000)>]
    #endif
    val mutable public N : int

    let mutable instance = Instance.create 0

    [<GlobalSetup>]
    member x.Setup() =
        instance <- Instance.create x.N


    [<Benchmark>]
    member x.HashMapOkasaki_containsKey() =
        let h = instance.okasaki
        for key in instance.existing do
            HashMapOkasaki.containsKey key h |> ignore

    [<Benchmark>]
    member x.ImTools_containsKey() =
        let h = instance.imtools
        for key in instance.existing do
            h.GetValueOrDefault(key, -128) |> ignore
        
        
    //[<Benchmark>]
    //member x.FSharpMap_containsKey() =
    //    let h = instance.fsharp
    //    for key in instance.existing do
    //        Map.containsKey key h |> ignore
        
    //[<Benchmark>]
    //member x.HAMT_containsKey() =
    //    let h = instance.hamt
    //    for key in instance.existing do
    //        h.ContainsKey key |> ignore
        
    //[<Benchmark>]
    //member x.FSharpX_containsKey() =
    //    let h = instance.fsharpx
    //    for key in instance.existing do
    //        PersistentHashMap.containsKey key h |> ignore
        
    [<Benchmark>]
    member x.ImmutableDictionary_containsKey() =
        let h = instance.system
        for key in instance.existing do
            h.ContainsKey key |> ignore

[<PlainExporter; MemoryDiagnoser; MaxIterationCount(Constants.maxIter)>]
type NegativeLookupBenchmark() =
    #if SMALL
    [<DefaultValue; Params(10000)>]
    #else
    [<DefaultValue; Params(10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000)>]
    #endif
    val mutable public N : int

    let mutable instance = Instance.create 0

    [<GlobalSetup>]
    member x.Setup() =
        instance <- Instance.create x.N


    [<Benchmark>]
    member x.HashMapOkasaki_containsKey() =
        let h = instance.okasaki
        for key in instance.nonExisting do
            HashMapOkasaki.containsKey key h |> ignore
        
        
    [<Benchmark>]
    member x.ImTools_containsKey() =
        let h = instance.imtools
        for key in instance.nonExisting do
            h.GetValueOrDefault(key, -128) |> ignore
        
        
    //[<Benchmark>]
    //member x.FSharpMap_containsKey() =
    //    let h = instance.fsharp
    //    for key in instance.nonExisting do
    //        Map.containsKey key h |> ignore
        
    //[<Benchmark>]
    //member x.HAMT_containsKey() =
    //    let h = instance.hamt
    //    for key in instance.nonExisting do
    //        h.ContainsKey key |> ignore
        
    //[<Benchmark>]
    //member x.FSharpX_containsKey() =
    //    let h = instance.fsharpx
    //    for key in instance.nonExisting do
    //        PersistentHashMap.containsKey key h |> ignore
        
    [<Benchmark>]
    member x.ImmutableDictionary_containsKey() =
        let h = instance.system
        for key in instance.nonExisting do
            h.ContainsKey key |> ignore




[<PlainExporter; MemoryDiagnoser; MaxIterationCount(Constants.maxIter)>]
type OfArrayBenchmark() =    
        
    #if SMALL
    [<DefaultValue; Params(10000)>]
    #else
    [<DefaultValue; Params(10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000)>]
    #endif
    val mutable public N : int

    let mutable list = [||]
    [<GlobalSetup>]
    member x.Setup() =
        list <- 
            [|1 .. x.N|] |> Array.map (fun i ->
                i, i*i
            )  

    [<Benchmark>]
    member x.HashMapOkasaki_ofArray() =
        HashMapOkasaki.ofArray list
        
    [<Benchmark>]
    member x.ImTools_ofArray() =
        let mutable res = ImTools.ImHashMap<int, int>.Empty
        for (k,v) in list do
            res <- res.AddOrUpdate(k, v)
        res
        
    [<Benchmark>]
    member x.FSharpMap_ofArray() =
        Map.ofArray list

    [<Benchmark>]
    member x.HAMT_ofArray() =
        let mutable res = HAMT.NET.V5.ImmutableDictionary<int, int>.Empty
        for (k,v) in list do
            res <- res.Add(k, v)
        res
        
    [<Benchmark>]
    member x.FSharpX_ofArray() =
        PersistentHashMap.ofSeq list
        
    [<Benchmark>]
    member x.ImmutableDictionary_ofArray() =
        let mutable res = ImmutableDictionary.Empty
        for (k,v) in list do
            res <- res.SetItem(k, v)
        res

[<PlainExporter; MemoryDiagnoser; MaxIterationCount(Constants.maxIter)>]
type ToArrayBenchmark() =    
        
    #if SMALL
    [<DefaultValue; Params(10000)>]
    #else
    [<DefaultValue; Params(10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000)>]
    #endif
    val mutable public N : int

    let mutable instance = Instance.create 0

    [<GlobalSetup>]
    member x.Setup() =
        instance <- Instance.create x.N

    [<Benchmark>]
    member x.HashMapOkasaki_toArray() =
        HashMapOkasaki.toArray instance.okasaki
        
    [<Benchmark>]
    member x.ImTools_toArray() =
        instance.imtools.Enumerate() |> Seq.toArray

    [<Benchmark>]
    member x.FSharpMap_toArray() =
        Map.toArray instance.fsharp
        
    [<Benchmark>]
    member x.ImmutableDictionary_toArray() =
        instance.system |> Seq.toArray



module RunTests =
    open System.Runtime.Intrinsics.X86

    let toCSV (res : BenchmarkDotNet.Reports.Summary) =
        let benchmarks = 
            res.Reports
            |> Seq.toList
            |> List.groupBy (fun c -> 
                let name = c.BenchmarkCase.Descriptor.WorkloadMethodDisplayInfo
                let n = c.BenchmarkCase.Parameters.["N"] |> unbox<int>
                (name, n)
            )
            |> Map.ofList
            |> Map.map (fun _ v -> List.head v)

        let sizes = benchmarks |> Map.toList |> List.map (fun ((_,n),_) -> n) |> Set.ofList
        let cols = benchmarks |> Map.toList |> List.map (fun ((n,_),_) -> n) |> Set.ofList |> Set.toList

        let builder = System.Text.StringBuilder()

        let names = cols |> String.concat ";"
        sprintf "N;%s" names |> builder.AppendLine |> ignore
        for s in sizes do
            let reports = 
                cols |> List.map (fun n ->
                    match Map.tryFind (n, s) benchmarks with
                    | Some r -> string r.ResultStatistics.Mean
                    | None -> ""
                )
            sprintf "%d;%s" s (String.concat ";" reports) |> builder.AppendLine |> ignore

        builder.ToString()

    open System
    open System.IO

    let runBenchmark<'T> (outPath : string) =
        //let cfg = BenchmarkDotNet.Configs.DebugInProcessConfig()
        let res = BenchmarkRunner.Run<'T>()
        let csv = toCSV res
        File.WriteAllText(outPath, csv)

    [<EntryPoint>]
    let main args =
        // run tests
        Expecto.Impl.runEval Expecto.Impl.ExpectoConfig.defaultConfig Tests.Tests.testSimpleTests
        |> Async.RunSynchronously
        |> ignore

        let outDir = 
            let outDir = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Desktop), "bench2")
            if not (Directory.Exists outDir) then Directory.CreateDirectory outDir |> ignore
            outDir
            
        ////Environment.CurrentDirectory <- outDir

        //runBenchmark<AddBenchmark> (Path.Combine(outDir, "add.csv"))
        //runBenchmark<UpdateBenchmark> (Path.Combine(outDir, "update.csv"))
        //runBenchmark<RemoveBenchmark> (Path.Combine(outDir, "remove.csv"))
        runBenchmark<PositiveLookupBenchmark> (Path.Combine(outDir, "lookup_work.csv"))
        runBenchmark<NegativeLookupBenchmark> (Path.Combine(outDir, "lookup_fail.csv"))
        //runBenchmark<OfArrayBenchmark> (Path.Combine(outDir, "ofArray.csv"))
        //runBenchmark<ToArrayBenchmark> (Path.Combine(outDir, "toArray.csv"))
        0

