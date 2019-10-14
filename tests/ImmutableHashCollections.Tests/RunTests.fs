namespace ImmutableHashCollections.Tests

open ImmutableHashCollections
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open System.Collections.Immutable
open BenchmarkDotNet.Reports
open FSharpx.Collections

[<PlainExporter>]
type UpdatePerformance() =
    let mutable okasaki = HashMapOkasaki.empty
    let mutable okasakiv = HashMapOkasakiVirtual.empty
    let mutable fsharpmap = Map.empty
    let mutable sys = ImmutableDictionary.Empty
    let mutable fsharpx = PersistentHashMap.empty
    let mutable key = 0

    [<DefaultValue; Params(0, 10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000)>]
    val mutable public N : int

    [<GlobalSetup>]
    member x.Setup() =
        okasaki <- HashMapOkasaki.ofList ([1..x.N] |> List.map (fun i -> i, i+1))
        okasakiv <- HashMapOkasakiVirtual.ofList ([1..x.N] |> List.map (fun i -> i, i+1))
        fsharpmap <- Map.ofList ([1..x.N] |> List.map (fun i -> i, i+1))
        fsharpx <- PersistentHashMap.ofSeq ([1..x.N] |> List.map (fun i -> i, i+1))
        sys <-
            (ImmutableDictionary.Empty, [1..x.N]) ||> List.fold (fun d k ->
                d.SetItem(k, k)
            )

        key <- x.N / 2

    [<Benchmark>]
    member x.HashMapOkasaki_update() =
        HashMapOkasaki.add key -123 okasaki
        
    //[<Benchmark>]
    //member x.FSharpX_update() =
    //    PersistentHashMap.add key -123 fsharpx
        
    [<Benchmark>]
    member x.HashMapOkasakiVirtual_update() =
        HashMapOkasakiVirtual.add key -123 okasakiv
        
    [<Benchmark>]
    member x.FSharpMap_update() =
        Map.add key -123 fsharpmap
        
    [<Benchmark>]
    member x.ImmutableDictionary_update() =
        sys.SetItem(key, -123)

[<PlainExporter>]
type AddPerformance() =
    let mutable okasaki = HashMapOkasaki.empty
    let mutable okasakiv = HashMapOkasakiVirtual.empty
    let mutable fsharpmap = Map.empty
    let mutable sys = ImmutableDictionary.Empty

    let mutable key = 0

    [<DefaultValue; Params(0, 10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000)>]
    val mutable public N : int

    [<GlobalSetup>]
    member x.Seup() =
        
        let list =
            [1.. x.N/2-1] @ [x.N/2+1 .. x.N] |> List.map (fun i ->
                i, i
            )
        okasaki <- HashMapOkasaki.ofList list
        okasakiv <- HashMapOkasakiVirtual.ofList list
        fsharpmap <- Map.ofList list
        sys <-
            (ImmutableDictionary.Empty, list) ||> List.fold (fun d (k,v) ->
                d.SetItem(k, v)
            )

        key <- x.N / 2

    [<Benchmark>]
    member x.HashMapOkasaki_add() =
        HashMapOkasaki.add key -123 okasaki
        
    [<Benchmark>]
    member x.HashMapOkasakiVirtual_add() =
        HashMapOkasakiVirtual.add key -123 okasakiv
        
    [<Benchmark>]
    member x.FSharpMap_add() =
        Map.add key -123 fsharpmap
        
    [<Benchmark>]
    member x.ImmutableDictionary_add() =
        sys.SetItem(key, -123)

[<PlainExporter>]
type RemovePerformance() =
    let mutable okasaki = HashMapOkasaki.empty
    let mutable okasakiv = HashMapOkasakiVirtual.empty
    let mutable fsharpmap = Map.empty
    let mutable sys = ImmutableDictionary.Empty

    let mutable key = 0

    [<DefaultValue; Params(0, 10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000)>]
    val mutable public N : int

    [<GlobalSetup>]
    member x.Seup() =
        
        let list =
            [1 .. x.N] |> List.map (fun i ->
                i, i
            )
        okasaki <- HashMapOkasaki.ofList list
        okasakiv <- HashMapOkasakiVirtual.ofList list
        fsharpmap <- Map.ofList list
        sys <-
            (ImmutableDictionary.Empty, list) ||> List.fold (fun d (k,v) ->
                d.SetItem(k, v)
            )

        key <- x.N / 2

    [<Benchmark>]
    member x.HashMapOkasaki_remove() =
        HashMapOkasaki.remove key okasaki
        
    [<Benchmark>]
    member x.HashMapOkasakiVirtual_remove() =
        HashMapOkasakiVirtual.remove key okasakiv
        
    [<Benchmark>]
    member x.FSharpMap_remove() =
        Map.remove key fsharpmap
        
    [<Benchmark>]
    member x.ImmutableDictionary_remove() =
        sys.Remove(key)

[<PlainExporter>]
type FailingLookupPerformance() =
    let mutable okasaki = HashMapOkasaki.empty
    let mutable okasakiv = HashMapOkasakiVirtual.empty
    let mutable fsharpmap = Map.empty
    let mutable sys = ImmutableDictionary.Empty

    let mutable key = 0

    [<DefaultValue; Params(0, 10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000)>]
    val mutable public N : int

    [<GlobalSetup>]
    member x.Seup() =
        
        let list =
            [1.. x.N/2-1] @ [x.N/2+1 .. x.N] |> List.map (fun i ->
                i, i
            )
        okasaki <- HashMapOkasaki.ofList list
        okasakiv <- HashMapOkasakiVirtual.ofList list
        fsharpmap <- Map.ofList list
        sys <-
            (ImmutableDictionary.Empty, list) ||> List.fold (fun d (k,v) ->
                d.SetItem(k, v)
            )

        key <- x.N / 2

    [<Benchmark>]
    member x.HashMapOkasaki_tryFind() =
        HashMapOkasaki.tryFind key okasaki
        
    [<Benchmark>]
    member x.HashMapOkasakiVirtual_tryFind() =
        HashMapOkasakiVirtual.tryFind key okasakiv
        
    [<Benchmark>]
    member x.FSharpMap_tryFind() =
        Map.tryFind key fsharpmap
        
    [<Benchmark>]
    member x.ImmutableDictionary_tryFind() =
        sys.TryGetValue(key)
 
[<PlainExporter>]
type WorkingLookupPerformance() =
    let mutable okasaki = HashMapOkasaki.empty
    let mutable okasakiv = HashMapOkasakiVirtual.empty
    let mutable fsharpmap = Map.empty
    let mutable sys = ImmutableDictionary.Empty

    let mutable key = 0

    [<DefaultValue; Params(0, 10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000)>]
    val mutable public N : int

    [<GlobalSetup>]
    member x.Setup() =
        
        let list =
            [1 .. x.N] |> List.map (fun i ->
                i, i*i
            )
        okasaki <- HashMapOkasaki.ofList list
        okasakiv <- HashMapOkasakiVirtual.ofList list
        fsharpmap <- Map.ofList list
        sys <-
            (ImmutableDictionary.Empty, list) ||> List.fold (fun d (k,v) ->
                d.SetItem(k, v)
            )

        key <- x.N / 2

    [<Benchmark>]
    member x.HashMapOkasaki_tryFind() =
        HashMapOkasaki.tryFind key okasaki
        
    [<Benchmark>]
    member x.HashMapOkasakiVirtual_tryFind() =
        HashMapOkasakiVirtual.tryFind key okasakiv
        
    [<Benchmark>]
    member x.FSharpMap_tryFind() =
        Map.tryFind key fsharpmap
        
    [<Benchmark>]
    member x.ImmutableDictionary_tryFind() =
        sys.TryGetValue(key)
       
      
[<PlainExporter>]
type OfListPerformance() =    
        
    [<DefaultValue; Params(0, 10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000)>]
    val mutable public N : int

    let mutable list = []
    [<GlobalSetup>]
    member x.Setup() =
        list <- 
            [1 .. x.N] |> List.map (fun i ->
                i, i*i
            )  

    [<Benchmark>]
    member x.HashMapOkasakiVirtual_ofList() =
        HashMapOkasakiVirtual.ofList list
        
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
        //let tree = HashMapOkasakiVirtual.ofList (List.map (fun i -> i, i) [ 1 .. 100000 ])

        //while true do
        //    HashMapOkasakiVirtual.tryFind 7231 tree |> ignore
        

        let outDir = 
            let outDir = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Desktop), "bench2")
            if not (Directory.Exists outDir) then Directory.CreateDirectory outDir |> ignore
            outDir
            
        Environment.CurrentDirectory <- outDir

        runBenchmark<WorkingLookupPerformance> (Path.Combine(outDir, "lookup_work.csv"))
        runBenchmark<FailingLookupPerformance> (Path.Combine(outDir, "lookup_fail.csv"))
        runBenchmark<RemovePerformance> (Path.Combine(outDir, "remove.csv"))
        runBenchmark<AddPerformance> (Path.Combine(outDir, "add.csv"))
        runBenchmark<UpdatePerformance> (Path.Combine(outDir, "update.csv"))
        runBenchmark<OfListPerformance> (Path.Combine(outDir, "ofList.csv"))

        //Tests.runTestsWithArgs defaultConfig args Tests.testSimpleTests |> ignore

        0

