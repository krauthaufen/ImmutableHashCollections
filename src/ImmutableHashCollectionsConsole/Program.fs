open ImmutableHashCollections


[<EntryPoint>]
let main argv = 
    let map = HashMapOkasaki.empty

    let m = 
        map
        |> HashMapOkasaki.add 0.0 "zero"
        |> HashMapOkasaki.add 1.0 "one"
        |> HashMapOkasaki.add 2.0 "two"
        |> HashMapOkasaki.add 1231231.0 "huge"
        |> HashMapOkasaki.add 1000.0 "thousand"
        |> HashMapOkasaki.add 4.0 "four"
        |> HashMapOkasaki.add 7.0 "seven"
        |> HashMapOkasaki.alter 1000.0 (fun o ->
            printfn "old: %A" o
            None
        )

    match HashMapOkasaki.tryRemove 4.0 m with
    | Some (value, rest) ->
        printfn "%A" value
        printfn "%A" (HashMapOkasaki.toList rest)
    | None ->
        ()

    printfn "%A %A" m.Count (HashMapOkasaki.toList m)
    0
