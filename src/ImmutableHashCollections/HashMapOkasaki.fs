namespace ImmutableHashCollections

#if OLD

open System.Collections
open System.Collections.Generic

[<AutoOpen>]
module internal HashMapOkasakiImplementation = 
    let inline mask (k : uint32) (m : uint32) = 
        //k &&& (m - 1u) // little endian
        (k ||| (m - 1u)) &&& ~~~m // big endian

    let inline matchPrefix (k : uint32) (p : uint32) (m : uint32) =
        mask k m = p

    let inline zeroBit (k : uint32) (m : uint32) =
        (k &&& m) = 0u

    let inline (==) (a : ^a) (b : ^a) =
        System.Object.ReferenceEquals(a, b)

    let inline highestBitMask x =
        let mutable x = x
        x <- x ||| (x >>> 1)
        x <- x ||| (x >>> 2)
        x <- x ||| (x >>> 4)
        x <- x ||| (x >>> 8)
        x <- x ||| (x >>> 16)
        x ^^^ (x >>> 1)

    let inline lowestBitMask x =
        x &&& (~~~x + 1u)

    let inline branchingBit (p0 : uint32) (p1 : uint32) =
        //lowestBitMask (p0 ^^^ p1) // little endian
        highestBitMask (p0 ^^^ p1) // big endian

    [<AllowNullLiteral>]
    type Linked<'K, 'V> =
        val mutable public Key : 'K
        val mutable public Value : 'V
        val mutable public Next : Linked<'K, 'V>

        new(k, v) = { Key = k; Value = v; Next = null }
        new(k, v, n) = { Key = k; Value = v; Next = n }

    module Linked =
        let rec add (cnt : ref<int>) (key : 'K) (value : 'V) (n : Linked<'K, 'V>) =
            if isNull n then
                cnt := !cnt + 1
                Linked(key, value)
            elif Unchecked.equals n.Key key then
                Linked(key, value, n.Next)
            else
                Linked(n.Key, n.Value, add cnt key value n.Next)
               
        let rec alter (cnt : ref<int>) (key : 'K) (update : option<'V> -> option<'V>) (n : Linked<'K, 'V>) =
            if isNull n then
                match update None with
                | Some value -> 
                    cnt := !cnt + 1
                    Linked(key, value)
                | None ->
                    null
            elif Unchecked.equals n.Key key then
                match update (Some n.Value) with
                | Some value -> 
                    Linked(key, value, n.Next)
                | None -> 
                    cnt := !cnt - 1
                    n.Next
            else
                Linked(n.Key, n.Value, alter cnt key update n.Next)
               
        let rec tryFind (key : 'K) (n : Linked<'K, 'V>) =
            if isNull n then None
            elif Unchecked.equals n.Key key then Some n.Value
            else tryFind key n.Next

        let destruct (n : Linked<'K, 'V>) =
            if isNull n then ValueNone
            else ValueSome(struct (n.Key, n.Value, n.Next))
            
        let rec remove (cnt : ref<int>) (key : 'K) (n : Linked<'K, 'V>) =
            if isNull n then
                null
            elif Unchecked.equals n.Key key then 
                cnt := !cnt - 1
                n.Next
            else
                let rest = remove cnt key n.Next
                if rest == n.Next then n
                else Linked(n.Key, n.Value, rest)

        let rec tryRemove (cnt : ref<int>) (key : 'K) (n : Linked<'K, 'V>) =
            if isNull n then
                None
            elif Unchecked.equals n.Key key then 
                cnt := !cnt - 1
                Some (n.Value, n.Next)
            else
                match tryRemove cnt key n.Next with
                | Some (value, rest) ->
                    Some(value, Linked(n.Key, n.Value, rest))
                | None ->
                    None

    [<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
    type HashMapOkasakiNode<'K, 'V> =
        | Empty
        | Leaf of hash : uint32 * key : 'K * value : 'V * rest : Linked<'K, 'V>
        | Node of prefix : uint32 * branchBit : uint32 * HashMapOkasakiNode<'K, 'V> * HashMapOkasakiNode<'K, 'V>
        
    module HashMapOkasakiNode =
        let inline private newNode (p : uint32) (m : uint32) (l : HashMapOkasakiNode<'K, 'V>) (r : HashMapOkasakiNode<'K, 'V>) =
            match l with
            | Empty -> r
            | _ ->
                match r with
                | Empty -> l
                | _ -> Node(p, m, l, r)

        let private join (p0 : uint32) (t0 : HashMapOkasakiNode<'K, 'V>) (p1 : uint32) (t1 : HashMapOkasakiNode<'K, 'V>) =
            let m = branchingBit p0 p1
            if zeroBit p0 m then Node(mask p0 m, m, t0, t1)
            else Node(mask p0 m, m, t1, t0)

        let rec tryFind (hash : uint32) (key : 'K) (node : HashMapOkasakiNode<'K, 'V>) =
            match node with
            | Empty -> None
            | Leaf(h, k, v, rest) -> 
                if h = hash then 
                    if Unchecked.equals k key then Some v
                    else Linked.tryFind key rest
                else
                    None
            | Node(prefix, mask, left, right) ->
                if matchPrefix hash prefix mask then
                    if zeroBit hash mask then tryFind hash key left
                    else tryFind hash key right
                else
                    None

        let rec add (cnt : ref<int>) (hash : uint32) (key : 'K) (value : 'V) (node : HashMapOkasakiNode<'K, 'V>) =
            match node with
            | Empty -> 
                cnt := !cnt + 1
                Leaf(hash, key, value, null)

            | Leaf(h, k, v, n) ->
                if h = hash then
                    if Unchecked.equals k key then 
                        Leaf(h, key, value, n)
                    else
                        Leaf(h, k, v, Linked.add cnt key value n)
                else
                    cnt := !cnt + 1
                    join h (node) hash (Leaf(hash, key, value, null))

            | Node(prefix, mask, left, right) ->
                if matchPrefix hash prefix mask then
                    if zeroBit hash mask then 
                        Node(prefix, mask, add cnt hash key value left, right)
                    else 
                        Node(prefix, mask, left, add cnt hash key value right)
                else
                    cnt := !cnt + 1
                    join prefix (node) hash (Leaf(hash, key, value, null))

        let rec tryRemove (cnt : ref<int>) (hash : uint32) (key : 'K) (node : HashMapOkasakiNode<'K, 'V>) =
            match node with
            | Empty -> 
                ValueNone

            | Leaf(h, k, v, n) ->
                if h = hash then
                    if Unchecked.equals k key then
                        cnt := !cnt - 1
                        match Linked.destruct n with
                        | ValueSome (struct (kn, vn, rest)) ->
                            ValueSome(v, Leaf(h, kn, vn, rest))
                        | ValueNone ->
                            ValueSome(v, Empty)
                    else
                        match Linked.tryRemove cnt key n with
                        | Some(v, r) ->
                            ValueSome(v, Leaf(h, k, v, r))
                        | None ->
                            ValueNone
                else 
                    ValueNone
            | Node(prefix, mask, left, right) ->
                if matchPrefix hash prefix mask then
                    if zeroBit hash mask then 
                        match tryRemove cnt hash key left with
                        | ValueSome(v, ll) ->
                            ValueSome(v, newNode prefix mask ll right)
                        | ValueNone -> 
                            ValueNone
                    else
                        match tryRemove cnt hash key right with
                        | ValueSome(v, rr) ->
                            ValueSome(v, newNode prefix mask left rr)
                        | ValueNone ->
                            ValueNone
                else
                    ValueNone

        let rec remove (cnt : ref<int>) (hash : uint32) (key : 'K) (node : HashMapOkasakiNode<'K, 'V>) =
            match node with
            | Empty -> node
            | Leaf(h, k, v, n) ->
                if h = hash then
                    if Unchecked.equals k key then
                        cnt := !cnt - 1
                        match Linked.destruct n with
                        | ValueSome (struct (k, v, rest)) ->
                            Leaf(h, k, v, rest)
                        | ValueNone ->
                            Empty
                    else
                        Leaf(h, k, v, Linked.remove cnt key n)
                else 
                    node
            | Node(prefix, mask, left, right) ->
                if matchPrefix hash prefix mask then
                    if zeroBit hash mask then 
                        let ll = remove cnt hash key left
                        if ll == left then node
                        else newNode prefix mask ll right
                    else
                        let rr = remove cnt hash key right
                        if rr == right then node
                        else newNode prefix mask left rr
                else
                    node

        let rec alter (cnt : ref<int>) (hash : uint32) (key : 'K) (update : option<'V> -> option<'V>) (node : HashMapOkasakiNode<'K, 'V>) =
            match node with
            | Empty ->
                match update None with
                | None -> 
                    node
                | Some v -> 
                    cnt := !cnt + 1
                    Leaf(hash, key, v, null)
            | Leaf(h, k, v, rest) ->
                if h = hash then
                    if Unchecked.equals k key then
                        match update (Some v) with
                        | Some v -> Leaf(h, k, v, rest)
                        | None -> 
                            cnt := !cnt - 1
                            match Linked.destruct rest with
                            | ValueSome (struct (k, v, rest)) ->
                                Leaf(h, k, v, rest)
                            | ValueNone ->
                                Empty
                    else
                        Leaf(h, k, v, Linked.alter cnt key update rest)
                else
                    match update None with
                    | None -> 
                        node
                    | Some v -> 
                        cnt := !cnt + 1
                        join h node hash (Leaf(hash, key, v, null))
            
            | Node(prefix, mask, left, right) ->
                if matchPrefix hash prefix mask then
                    if zeroBit hash mask then 
                        Node(prefix, mask, alter cnt hash key update left, right)
                    else 
                        Node(prefix, mask, left, alter cnt hash key update right)
                else
                    match update None with
                    | None -> 
                        node
                    | Some v -> 
                        cnt := !cnt + 1
                        join prefix (node) hash (Leaf(hash, key, v, null))

[<Struct>]
type HashMapOkasaki<'K, 'V> internal(cnt : int, root : HashMapOkasakiNode<'K, 'V>) =

    static member Empty = HashMapOkasaki<'K, 'V>(0, HashMapOkasakiNode.Empty)
    
    member x.Count = cnt
    member internal x.Root = root

    member x.Add(key : 'K, value : 'V) =
        let c = ref cnt
        let hash = Unchecked.hash key |> uint32
        let newRoot = root |> HashMapOkasakiNode.add c hash key value
        HashMapOkasaki(!c, newRoot)
        
    member x.Remove(key : 'K) =
        let c = ref cnt
        let hash = Unchecked.hash key |> uint32
        let newRoot = root |> HashMapOkasakiNode.remove c hash key
        HashMapOkasaki(!c, newRoot)
        
    member x.Alter(key : 'K, update : option<'V> -> option<'V>) =
        let c = ref cnt
        let hash = Unchecked.hash key |> uint32
        let newRoot = root |> HashMapOkasakiNode.alter c hash key update
        HashMapOkasaki(!c, newRoot)

    member x.TryRemove(key : 'K) =
        let c = ref cnt
        let hash = Unchecked.hash key |> uint32
        match HashMapOkasakiNode.tryRemove c hash key root with
        | ValueSome(value, root) ->
            Some(value, HashMapOkasaki(!c, root))
        | ValueNone ->
            None
        
    member x.TryFind(key : 'K) =
        let hash = Unchecked.hash key |> uint32
        HashMapOkasakiNode.tryFind hash key root

    interface IEnumerable with
        member x.GetEnumerator() = new HashMapOkasakiEnumerator<'K, 'V>(root) :> _

    interface IEnumerable<'K * 'V> with
        member x.GetEnumerator() = new HashMapOkasakiEnumerator<'K, 'V>(root) :> _

and private HashMapOkasakiEnumerator<'K, 'V>(root : HashMapOkasakiNode<'K, 'V>) =
    let mutable stack = [ root ]
    let mutable current = Unchecked.defaultof<'K * 'V>
    let mutable rest : Linked<'K, 'V> = null

    member x.MoveNext() =
        if isNull rest then
            match stack with
            | HashMapOkasakiNode.Leaf(h, k, v, ns) :: r ->
                stack <- r
                current <- (k, v)
                rest <- ns
                true
            | HashMapOkasakiNode.Empty :: r ->
                stack <- r
                x.MoveNext()
            | HashMapOkasakiNode.Node(_,_,l,r) :: rest ->
                stack <- l :: r :: rest
                x.MoveNext()
            | [] ->
                false
        else
            current <- rest.Key, rest.Value
            rest <- rest.Next
            true
            
    member x.Current = current

    interface IEnumerator with
        member x.MoveNext() = x.MoveNext()
        member x.Reset() =
            current <- Unchecked.defaultof<_>
            rest <- null
            stack <- [root]
        member x.Current = x.Current :> obj

    interface IEnumerator<'K * 'V> with
        member x.Dispose() = 
            current <- Unchecked.defaultof<_>
            rest <- null
            stack <- []
        member x.Current = x.Current

module HashMapOkasaki =

    [<GeneralizableValue>]
    let empty<'K, 'V> = HashMapOkasaki<'K, 'V>.Empty

    let ofSeq (seq : seq<'K * 'V>) =
        let mutable res = empty
        for (k, v) in seq do res <- res.Add(k, v)
        res

    let inline ofList (list : list<'K * 'V>) = 
        ofSeq list

    let inline ofArray (arr : array<'K * 'V>) = 
        ofSeq arr

    let inline toSeq (m : HashMapOkasaki<'K, 'V>) = m :> seq<_>
    let inline toList (m : HashMapOkasaki<'K, 'V>) = m |> Seq.toList
    let inline toArray (m : HashMapOkasaki<'K, 'V>) = m |> Seq.toArray

    let inline add (key : 'K) (value : 'V) (map : HashMapOkasaki<'K, 'V>) =
        map.Add(key, value)

    let inline remove (key : 'K) (map : HashMapOkasaki<'K, 'V>) =
        map.Remove(key)
        
    let inline alter (key : 'K) (update : option<'V> -> option<'V>) (map : HashMapOkasaki<'K, 'V>) =
        map.Alter(key, update)

    let inline tryRemove (key : 'K) (map : HashMapOkasaki<'K, 'V>) =
        map.TryRemove(key)
        
    let inline tryFind (key : 'K) (map : HashMapOkasaki<'K, 'V>) =
        map.TryFind(key)

#endif