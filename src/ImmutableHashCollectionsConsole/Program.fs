open ImmutableHashCollections


[<EntryPoint>]
let main argv = 
    let map = HashMapOkasakiVirtual.empty

    let m = 
        map
        |> HashMapOkasakiVirtual.add 0.0 "zero"
        |> HashMapOkasakiVirtual.add 1.0 "one"
        |> HashMapOkasakiVirtual.add 2.0 "two"
        |> HashMapOkasakiVirtual.add 1231231.0 "huge"
        |> HashMapOkasakiVirtual.add 1000.0 "thousand"
        |> HashMapOkasakiVirtual.add 4.0 "four"
        |> HashMapOkasakiVirtual.add 7.0 "seven"
        |> HashMapOkasakiVirtual.alter 1000.0 (fun o ->
            printfn "old: %A" o
            None
        )

    match HashMapOkasakiVirtual.tryRemove 4.0 m with
    | Some (value, rest) ->
        printfn "%A" value
        printfn "%A" (HashMapOkasakiVirtual.toList rest)
    | None ->
        ()

    printfn "%A %A" m.Count (HashMapOkasakiVirtual.toList m)
    0
