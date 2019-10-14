namespace ImmutableHashCollections.Tests

open Expecto
open FsCheck
open ImmutableHashCollections

module Tests =
    let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000}
    // bug somewhere:  registering arbitrary generators causes Expecto VS test adapter not to work
    //let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000; arbitrary = [typeof<Generators>] }
    let configReplay = { FsCheckConfig.defaultConfig with maxTest = 10000 ; replay = Some <| (1940624926, 296296394) }

    module Expect =
        let equalMaps (m : Map<'a, 'b>) (hm : HashMapOkasakiVirtual<'a, 'b>) =
            let a = hm |> HashMapOkasakiVirtual.toList |> List.sortBy fst
            let b = m |> Map.toList
            
            Expect.equal (Map.count m) (hm.Count) "Expect Equal Count"
            Expect.equal a b "Expected Equal Maps"

            for (k, v) in b do
                match HashMapOkasakiVirtual.tryFind k hm with
                | Some vh -> Expect.equal v vh "bad value"
                | None -> failwith "bad"

            
    module Map =
        let alter (key : 'K) (update : option<'V> -> option<'V>) (m : Map<'K, 'V>) =
            let o = Map.tryFind key m
            match update o with
            | None -> Map.remove key m
            | Some n -> Map.add key n m



    [<Tests>]
    let testSimpleTests =

        testList "HashMapOkasakiVirtual" [
            testPropertyWithConfig config10k "add" <| fun (m : Map<string, string>) ->
                let hm = HashMapOkasakiVirtual.ofSeq (Map.toSeq m)

                let hm = HashMapOkasakiVirtual.add "a" "b" hm
                let m = Map.add "a" "b" m
                Expect.equalMaps m hm

            testPropertyWithConfig config10k "remove" <| fun (m : Map<string, string>) ->
                let hm = HashMapOkasakiVirtual.ofSeq (Map.toSeq m)

                let hm = HashMapOkasakiVirtual.remove "a" hm
                let m = Map.remove "a" m
                Expect.equalMaps m hm

                match m |> Map.toSeq |> Seq.tryHead with
                | Some (k,_) ->
                    let hm = HashMapOkasakiVirtual.remove k hm
                    let m = Map.remove k m
                    Expect.equalMaps m hm
                | None ->
                    ()
                    
            testProperty "alter" <| fun (m : Map<string, string>) ->
                let hm = HashMapOkasakiVirtual.ofSeq (Map.toSeq m)

                let hm = HashMapOkasakiVirtual.alter "a" (fun o -> Some "a") hm
                let m = Map.alter "a"  (fun o -> Some "a") m
                Expect.equalMaps m hm

                match m |> Map.toSeq |> Seq.tryHead with
                | Some (k,_) ->
                    let hm1 = HashMapOkasakiVirtual.alter k (function Some o -> None | _ -> failwith "bad") hm
                    let m1 = Map.alter k (function Some o -> None | _ -> failwith "bad") m
                    Expect.equalMaps m1 hm1

                    let hm1 = HashMapOkasakiVirtual.alter k (function Some o -> Some (o + "a") | _ -> failwith "bad") hm
                    let m1 = Map.alter k (function Some o -> Some (o + "a") | _ -> failwith "bad") m
                    Expect.equalMaps m1 hm1

                | None ->
                    ()

        ]

