namespace ImmutableHashCollections

open System.Collections
open System.Collections.Generic

[<AutoOpen>]
module internal HashMapOkasakiVirtualImplementation = 
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
        let rec add (key : 'K) (value : 'V) (n : Linked<'K, 'V>) =
            if isNull n then
                Linked(key, value)
            elif Unchecked.equals n.Key key then
                Linked(key, value, n.Next)
            else
                Linked(n.Key, n.Value, add key value n.Next)
               
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

    [<AbstractClass>]
    type HashMapOkasakiNode<'K, 'V>() =
        abstract member Add : uint32 * 'K * 'V -> HashMapOkasakiNode<'K, 'V>

    type HashMapOkasakiEmptyNode<'K, 'V> private() =
        inherit HashMapOkasakiNode<'K, 'V>()
        static let instance = HashMapOkasakiEmptyNode<'K, 'V>() :> HashMapOkasakiNode<_,_>
        static member Instance = instance

        override x.Add(hash : uint32, key : 'K, value : 'V) =
            HashMapOkasakiLeafNode<'K, 'V>(hash, key, value, null) :> _

    and HashMapOkasakiLeafNode<'K, 'V> =
        inherit HashMapOkasakiNode<'K, 'V>
        val mutable public Hash : uint32
        val mutable public Key : 'K
        val mutable public Value : 'V
        val mutable public Next : Linked<'K, 'V>

        override x.Add(hash : uint32, key : 'K, value : 'V) =
            if x.Hash = hash then
                if Unchecked.equals key x.Key then
                    HashMapOkasakiLeafNode<'K, 'V>(x.Hash, key, value, x.Next) :> _
                else
                    HashMapOkasakiLeafNode<'K, 'V>(x.Hash, x.Key, x.Value, Linked.add key value x.Next) :> _
            else
                let n = HashMapOkasakiLeafNode<'K, 'V>(hash, key, value, null)
                HashMapOkasakiInnerNode.Join(hash, n, x.Hash, x)

        new(h : uint32, k : 'K, v : 'V, n : Linked<'K, 'V>) = { inherit HashMapOkasakiNode<'K, 'V>(); Hash = h; Key = k; Value = v; Next = n }
        
    and HashMapOkasakiInnerNode<'K, 'V> =
        inherit HashMapOkasakiNode<'K, 'V>
        val mutable public Prefix : uint32
        val mutable public Mask : uint32
        val mutable public Left : HashMapOkasakiNode<'K, 'V>
        val mutable public Right : HashMapOkasakiNode<'K, 'V>
        
        override x.Add(hash : uint32, key : 'K, value : 'V) =
            if matchPrefix hash x.Prefix x.Mask then
                if zeroBit hash x.Mask then 
                    HashMapOkasakiInnerNode(x.Prefix, x.Mask, x.Left.Add(hash, key, value), x.Right) :> _
                else 
                    HashMapOkasakiInnerNode(x.Prefix, x.Mask, x.Left, x.Right.Add(hash, key, value)) :> _
            else
                HashMapOkasakiInnerNode.Join(x.Prefix, x, hash, HashMapOkasakiLeafNode(hash, key, value, null))

        static member Join (p0 : uint32, t0 : HashMapOkasakiNode<'K, 'V>, p1 : uint32, t1 : HashMapOkasakiNode<'K, 'V>) : HashMapOkasakiNode<'K,'V>=
            let m = branchingBit p0 p1
            if zeroBit p0 m then HashMapOkasakiInnerNode(mask p0 m, m, t0, t1) :> HashMapOkasakiNode<_,_>
            else HashMapOkasakiInnerNode(mask p0 m, m, t1, t0) :> HashMapOkasakiNode<_,_>


        new(p : uint32, m : uint32, l : HashMapOkasakiNode<'K, 'V>, r : HashMapOkasakiNode<'K, 'V>) = 
            { inherit HashMapOkasakiNode<'K, 'V>(); Prefix = p; Mask = m; Left = l; Right = r }
        


[<Struct>]
type HashMapOkasakiVirtual<'K, 'V> internal(root : HashMapOkasakiNode<'K, 'V>) =

    static member Empty = HashMapOkasakiVirtual<'K, 'V>(HashMapOkasakiEmptyNode.Instance)
    
    member internal x.Root = root

    member x.Add(key : 'K, value : 'V) =
        let hash = Unchecked.hash key |> uint32
        let newRoot = root.Add(hash, key, value)
        HashMapOkasakiVirtual newRoot


module HashMapOkasakiVirtual =

    [<GeneralizableValue>]
    let empty<'K, 'V> = HashMapOkasakiVirtual<'K, 'V>.Empty

    let ofSeq (seq : seq<'K * 'V>) =
        let mutable res = empty
        for (k, v) in seq do res <- res.Add(k, v)
        res

    let inline ofList (list : list<'K * 'V>) = 
        ofSeq list

    let inline ofArray (arr : array<'K * 'V>) = 
        ofSeq arr

    let inline add (key : 'K) (value : 'V) (map : HashMapOkasakiVirtual<'K, 'V>) =
        map.Add(key, value)