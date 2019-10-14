namespace ImmutableHashCollections.Tests

open ImmutableHashCollections
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open System.Collections.Immutable
open BenchmarkDotNet.Reports
open FSharpx.Collections

[<PlainExporter>]
type UpdatePerformance() =
    let mutable okasakiv = HashMapOkasaki.empty
    let mutable fsharpmap = Map.empty
    let mutable sys = ImmutableDictionary.Empty
    let mutable fsharpx = PersistentHashMap.empty
    let mutable key = 0

    #if SMALL
    [<DefaultValue; Params(10000)>]
    #else
    [<DefaultValue; Params(0, 10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000)>]
    #endif
    val mutable public N : int

    [<GlobalSetup>]
    member x.Setup() =
        okasakiv <- HashMapOkasaki.ofList ([1..x.N] |> List.map (fun i -> i, i+1))
        fsharpmap <- Map.ofList ([1..x.N] |> List.map (fun i -> i, i+1))
        fsharpx <- PersistentHashMap.ofSeq ([1..x.N] |> List.map (fun i -> i, i+1))
        sys <-
            (ImmutableDictionary.Empty, [1..x.N]) ||> List.fold (fun d k ->
                d.SetItem(k, k)
            )

        key <- x.N / 2

    [<Benchmark>]
    member x.FSharpX_update() =
        PersistentHashMap.add key -123 fsharpx
        
    [<Benchmark>]
    member x.HashMapOkasaki_update() =
        HashMapOkasaki.add key -123 okasakiv
        
    [<Benchmark>]
    member x.FSharpMap_update() =
        Map.add key -123 fsharpmap
        
    [<Benchmark>]
    member x.ImmutableDictionary_update() =
        sys.SetItem(key, -123)

[<PlainExporter>]
type AddPerformance() =
    let mutable okasakiv = HashMapOkasaki.empty
    let mutable fsharpmap = Map.empty
    let mutable sys = ImmutableDictionary.Empty
    let mutable fsharpx = PersistentHashMap.empty

    let mutable key = 0

    #if SMALL
    [<DefaultValue; Params(10000)>]
    #else
    [<DefaultValue; Params(0, 10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000)>]
    #endif
    val mutable public N : int

    [<GlobalSetup>]
    member x.Seup() =
        
        let list =
            [1.. x.N/2-1] @ [x.N/2+1 .. x.N] |> List.map (fun i ->
                i, i
            )
        okasakiv <- HashMapOkasaki.ofList list
        fsharpmap <- Map.ofList list
        fsharpx <- PersistentHashMap.ofSeq list
        sys <-
            (ImmutableDictionary.Empty, list) ||> List.fold (fun d (k,v) ->
                d.SetItem(k, v)
            )

        key <- x.N / 2

    [<Benchmark>]
    member x.HashMapOkasaki_add() =
        HashMapOkasaki.add key -123 okasakiv
        
    [<Benchmark>]
    member x.FSharpMap_add() =
        Map.add key -123 fsharpmap
      
    [<Benchmark>]
    member x.FSharpX_add() =
        PersistentHashMap.add key -123 fsharpx
            
    [<Benchmark>]
    member x.ImmutableDictionary_add() =
        sys.SetItem(key, -123)

[<PlainExporter>]
type RemovePerformance() =
    let mutable okasakiv = HashMapOkasaki.empty
    let mutable fsharpmap = Map.empty
    let mutable sys = ImmutableDictionary.Empty
    let mutable fsharpx = PersistentHashMap.empty

    let mutable key = 0

    #if SMALL
    [<DefaultValue; Params(10000)>]
    #else
    [<DefaultValue; Params(0, 10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000)>]
    #endif
    val mutable public N : int

    [<GlobalSetup>]
    member x.Seup() =
        
        let list =
            [1 .. x.N] |> List.map (fun i ->
                i, i
            )
        okasakiv <- HashMapOkasaki.ofList list
        fsharpmap <- Map.ofList list
        sys <-
            (ImmutableDictionary.Empty, list) ||> List.fold (fun d (k,v) ->
                d.SetItem(k, v)
            )
        fsharpx <- PersistentHashMap.ofSeq list
        key <- x.N / 2

    [<Benchmark>]
    member x.HashMapOkasaki_remove() =
        HashMapOkasaki.remove key okasakiv
        
    [<Benchmark>]
    member x.FSharpMap_remove() =
        Map.remove key fsharpmap
        
    [<Benchmark>]
    member x.FSharpX_remove() =
        PersistentHashMap.remove key fsharpx
        
    [<Benchmark>]
    member x.ImmutableDictionary_remove() =
        sys.Remove(key)

[<PlainExporter>]
type FailingLookupPerformance() =
    let mutable okasakiv = HashMapOkasaki.empty
    let mutable fsharpmap = Map.empty
    let mutable sys = ImmutableDictionary.Empty
    let mutable fsharpx = PersistentHashMap.empty

    let mutable key = 0

    #if SMALL
    [<DefaultValue; Params(10000)>]
    #else
    [<DefaultValue; Params(0, 10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000)>]
    #endif
    val mutable public N : int

    [<GlobalSetup>]
    member x.Seup() =
        
        let list =
            [1.. x.N/2-1] @ [x.N/2+1 .. x.N] |> List.map (fun i ->
                i, i
            )
        okasakiv <- HashMapOkasaki.ofList list
        fsharpmap <- Map.ofList list
        fsharpx <- PersistentHashMap.ofSeq list
        sys <-
            (ImmutableDictionary.Empty, list) ||> List.fold (fun d (k,v) ->
                d.SetItem(k, v)
            )

        key <- x.N / 2

    [<Benchmark>]
    member x.HashMapOkasaki_tryFind() =
        HashMapOkasaki.tryFind key okasakiv
        
    [<Benchmark>]
    member x.FSharpMap_tryFind() =
        Map.tryFind key fsharpmap
        
    [<Benchmark>]
    member x.FSharpX_containsKey() =
        PersistentHashMap.containsKey key fsharpx
        
    [<Benchmark>]
    member x.ImmutableDictionary_tryFind() =
        sys.TryGetValue(key)
 
[<PlainExporter>]
type WorkingLookupPerformance() =
    let mutable okasakiv = HashMapOkasaki.empty
    let mutable fsharpmap = Map.empty
    let mutable sys = ImmutableDictionary.Empty
    let mutable fsharpx = PersistentHashMap.empty

    let mutable key = 0

    #if SMALL
    [<DefaultValue; Params(10000)>]
    #else
    [<DefaultValue; Params(0, 10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000)>]
    #endif
    val mutable public N : int

    [<GlobalSetup>]
    member x.Setup() =
        
        let list =
            [1 .. x.N] |> List.map (fun i ->
                i, i*i
            )
        okasakiv <- HashMapOkasaki.ofList list
        fsharpmap <- Map.ofList list
        fsharpx <- PersistentHashMap.ofSeq list
        sys <-
            (ImmutableDictionary.Empty, list) ||> List.fold (fun d (k,v) ->
                d.SetItem(k, v)
            )

        key <- x.N / 2

    [<Benchmark>]
    member x.HashMapOkasaki_tryFind() =
        HashMapOkasaki.tryFind key okasakiv
        
    [<Benchmark>]
    member x.FSharpMap_tryFind() =
        Map.tryFind key fsharpmap
        
    [<Benchmark>]
    member x.FSharpX_containsKey() =
        PersistentHashMap.containsKey key fsharpx
        
    [<Benchmark>]
    member x.ImmutableDictionary_tryFind() =
        sys.TryGetValue(key)

[<PlainExporter>]
type OfListPerformance() =    
        
    #if SMALL
    [<DefaultValue; Params(10000)>]
    #else
    [<DefaultValue; Params(0, 10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000)>]
    #endif
    val mutable public N : int

    let mutable list = []
    [<GlobalSetup>]
    member x.Setup() =
        list <- 
            [1 .. x.N] |> List.map (fun i ->
                i, i*i
            )  

    [<Benchmark>]
    member x.HashMapOkasaki_ofList() =
        HashMapOkasaki.ofList list
        
    [<Benchmark>]
    member x.HashMapOkasaki_ofListUnoptimized() =
        HashMapOkasaki.ofListUnoptimized list
        
    [<Benchmark>]
    member x.FSharpX_ofList() =
        PersistentHashMap.ofSeq list
        
    [<Benchmark>]
    member x.FSharpMap_ofList() =
        Map.ofList list
        


module RunTests =

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
        let res = BenchmarkRunner.Run<'T>()
        let csv = toCSV res
        File.WriteAllText(outPath, csv)

    [<EntryPoint>]
    let main args =
        let res = 
            Expecto.Impl.runEval Expecto.Impl.ExpectoConfig.defaultConfig Tests.Tests.testSimpleTests
            |> Async.RunSynchronously

        let outDir = 
            let outDir = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Desktop), "bench2")
            if not (Directory.Exists outDir) then Directory.CreateDirectory outDir |> ignore
            outDir
            
        //Environment.CurrentDirectory <- outDir

        //runBenchmark<WorkingLookupPerformance> (Path.Combine(outDir, "lookup_work.csv"))
        //runBenchmark<FailingLookupPerformance> (Path.Combine(outDir, "lookup_fail.csv"))
        //runBenchmark<RemovePerformance> (Path.Combine(outDir, "remove.csv"))
        //runBenchmark<AddPerformance> (Path.Combine(outDir, "add.csv"))
        //runBenchmark<UpdatePerformance> (Path.Combine(outDir, "update.csv"))
        runBenchmark<OfListPerformance> (Path.Combine(outDir, "ofList.csv"))
        0

