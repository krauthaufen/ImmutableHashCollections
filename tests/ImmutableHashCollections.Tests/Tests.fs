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
        let equalHashMaps (m : HashMapOkasaki<'a, 'b>) (hm : HashMapOkasaki<'a, 'b>) =
            let a = hm |> HashMapOkasaki.toList |> List.sortBy fst
            let b = m  |> HashMapOkasaki.toList |> List.sortBy fst

            Expect.equal (List.length b) (hm.Count) "Expect Equal Count"
            Expect.equal a b "Expected Equal Maps"

            for (k, v) in b do
                match HashMapOkasaki.tryFind k hm with
                | Some vh -> Expect.equal v vh "bad value"
                | None -> failwith "bad"
        let equalMaps (m : Map<'a, 'b>) (hm : HashMapOkasaki<'a, 'b>) =
            let a = hm |> HashMapOkasaki.toList |> List.sortBy fst
            let b = m |> Map.toList
            
            Expect.equal (Map.count m) (hm.Count) "Expect Equal Count"
            Expect.equal a b "Expected Equal Maps"

            for (k, v) in b do
                match HashMapOkasaki.tryFind k hm with
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

        testList "HashMapOkasaki" [
            testPropertyWithConfig config10k "add" <| fun (m : Map<string, string>) ->
                let hm = HashMapOkasaki.ofSeq (Map.toSeq m)

                let hm = HashMapOkasaki.add "a" "b" hm
                let m = Map.add "a" "b" m
                Expect.equalMaps m hm

            testPropertyWithConfig config10k "remove" <| fun (m : Map<string, string>) ->
                let hm = HashMapOkasaki.ofSeq (Map.toSeq m)

                let hm = HashMapOkasaki.remove "a" hm
                let m = Map.remove "a" m
                Expect.equalMaps m hm

                match m |> Map.toSeq |> Seq.tryHead with
                | Some (k,_) ->
                    let hm = HashMapOkasaki.remove k hm
                    let m = Map.remove k m
                    Expect.equalMaps m hm
                | None ->
                    ()
                    
            testPropertyWithConfig config10k "tryRemove" <| fun (m : Map<string, string>) ->
                let hm = HashMapOkasaki.ofSeq (Map.toSeq m)

                match m |> Map.toSeq |> Seq.tryHead with
                | Some (k,mv) ->
                    match HashMapOkasaki.tryRemove k hm with
                    | Some (value, rest) ->
                        Expect.equalMaps (Map.remove k m) rest
                        Expect.equal value mv "bad value"

                    | None ->
                        failwith "bad"
                | None ->
                    ()
            testProperty "alter" <| fun (m : Map<string, string>) ->
                let hm = HashMapOkasaki.ofSeq (Map.toSeq m)

                let hm = HashMapOkasaki.alter "a" (fun o -> Some "a") hm
                let m = Map.alter "a"  (fun o -> Some "a") m
                Expect.equalMaps m hm

                match m |> Map.toSeq |> Seq.tryHead with
                | Some (k,_) ->
                    let hm1 = HashMapOkasaki.alter k (function Some o -> None | _ -> failwith "bad") hm
                    let m1 = Map.alter k (function Some o -> None | _ -> failwith "bad") m
                    Expect.equalMaps m1 hm1

                    let hm1 = HashMapOkasaki.alter k (function Some o -> Some (o + "a") | _ -> failwith "bad") hm
                    let m1 = Map.alter k (function Some o -> Some (o + "a") | _ -> failwith "bad") m
                    Expect.equalMaps m1 hm1

                | None ->
                    ()

            testProperty "compute/applyDelta" <| fun (ma : Map<string, string>) (mb : Map<string, string>) ->
                let a = HashMapOkasaki.ofSeq (Map.toSeq ma)
                let b = HashMapOkasaki.ofSeq (Map.toSeq mb)
                
                let aa = HashMapOkasaki.computeDelta a a
                Expect.sequenceEqual aa Seq.empty "diff(a,a) <> empty"
                
                let bb = HashMapOkasaki.computeDelta b b
                Expect.sequenceEqual bb Seq.empty "diff(b,b) <> empty"

                let ab = HashMapOkasaki.computeDelta a b
                let ba = HashMapOkasaki.computeDelta b a

                let b1, ab1 = HashMapOkasaki.applyDelta a ab
                Expect.equalHashMaps b b1
                Expect.equalHashMaps ab ab1

                let a1, ba1 = HashMapOkasaki.applyDelta b ba
                Expect.equalHashMaps a a1
                Expect.equalHashMaps ba ba1




        ]

