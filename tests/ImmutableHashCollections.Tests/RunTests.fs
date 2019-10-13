namespace ImmutableHashCollections.Tests

open ImmutableHashCollections
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

type Bench() =
    let map = HashMap.ofList ([1..100] |> List.map (fun i -> i, i+1))

    [<GlobalSetup>]
    member x.Seup() =
        printfn "setup"
        ()
    [<Benchmark>]
    member x.Add() =
        HashMap.add 5123 123 map

        


module RunTests =
    let runBenchmark() =
        let cfg = BenchmarkDotNet.Configs.DebugInProcessConfig()
        let res = BenchmarkRunner.Run<Bench>(cfg)
        printfn "bad: %A" res.BenchmarksCases
        printfn "%s" (string res)

    [<EntryPoint>]
    let main args =
        runBenchmark() |> ignore

        //Tests.runTestsWithArgs defaultConfig args Tests.testSimpleTests |> ignore

        0

