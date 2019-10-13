namespace ImmutableHashCollections.Tests

open ImmutableHashCollections
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

type AddPerformance() =
    let mutable okasaki = HashMapOkasaki.empty
    let mutable fsharpmap = Map.empty
    let mutable key = 0

    [<DefaultValue; Params(0, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000)>]
    val mutable public N : int

    [<GlobalSetup>]
    member x.Seup() =
        okasaki <- HashMapOkasaki.ofList ([1..x.N] |> List.map (fun i -> i, i+1))
        fsharpmap <- Map.ofList ([1..x.N] |> List.map (fun i -> i, i+1))
        key <- x.N / 2

    [<Benchmark>]
    member x.HashMapOkasaki_add() =
        HashMapOkasaki.add key key okasaki
        
    [<Benchmark>]
    member x.FSharpMap_add() =
        Map.add key key fsharpmap

        
        


module RunTests =
    let runBenchmark() =
        let res = BenchmarkRunner.Run<AddPerformance>()

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

        let names = cols |> String.concat ";"
        printfn "N;%s" names
        for s in sizes do
            let reports = 
                cols |> List.map (fun n ->
                    match Map.tryFind (n, s) benchmarks with
                    | Some r -> string r.ResultStatistics.Mean
                    | None -> ""
                )
            printfn "%d;%s" s (String.concat ";" reports)


    [<EntryPoint>]
    let main args =
        runBenchmark() |> ignore

        //Tests.runTestsWithArgs defaultConfig args Tests.testSimpleTests |> ignore

        0

