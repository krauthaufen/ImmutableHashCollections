open ImmutableHashCollections


[<EntryPoint>]
let main argv = 
    let map = HashMap.empty

    let m = 
        map
        |> HashMap.add 0.0 "zero"
        |> HashMap.add 1.0 "one"
        |> HashMap.add 2.0 "two"
        |> HashMap.add 1231231.0 "huge"
        |> HashMap.add 1000.0 "thousand"
        |> HashMap.add 4.0 "four"
        |> HashMap.add 7.0 "seven"
        |> HashMap.alter 1000.0 (fun o ->
            printfn "old: %A" o
            None
        )

    match HashMap.tryRemove 4.0 m with
    | Some (value, rest) ->
        printfn "%A" value
        printfn "%A" (HashMap.toList rest)
    | None ->
        ()

    printfn "%A %A" m.Count (HashMap.toList m)
    0
