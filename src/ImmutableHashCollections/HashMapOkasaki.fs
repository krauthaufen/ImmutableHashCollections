namespace ImmutableHashCollections

open System.Collections
open System.Collections.Generic
open System.Runtime.CompilerServices
#if NETCOREAPP3_0 && USE_INTRINSICS
open System.Runtime.Intrinsics.X86
#endif

[<AutoOpen>]
module internal HashMapOkasakiImplementation = 

    type Mask = uint32

    let inline private highestBitMask x =
        let mutable x = x
        x <- x ||| (x >>> 1)
        x <- x ||| (x >>> 2)
        x <- x ||| (x >>> 4)
        x <- x ||| (x >>> 8)
        x <- x ||| (x >>> 16)
        x ^^^ (x >>> 1)

    let inline getPrefix (k: uint32) (m: Mask) = 
        #if NETCOREAPP3_0 && USE_INTRINSICS
        if Bmi1.IsSupported then
            k
        else
            k &&& ~~~((m <<< 1) - 1u)
        #else
        k &&& ~~~((m <<< 1) - 1u)
        #endif

    #if NETCOREAPP3_0 && USE_INTRINSICS
    let inline zeroBit (k: uint32) (m: Mask) =
        if Bmi1.IsSupported then
            Bmi1.BitFieldExtract(k, uint16 m)
        else
            if (k &&& m) <> 0u then 1u else 0u
    #else
    let inline zeroBit (k: uint32) (m: uint32) =
        if (k &&& m) <> 0u then 1u else 0u
    #endif
        
    #if NETCOREAPP3_0 && USE_INTRINSICS 
    let inline matchPrefixAndGetBit (hash: uint32) (prefix: uint32) (m: Mask) =
        if Bmi1.IsSupported then
            let lz = Lzcnt.LeadingZeroCount (hash ^^^ prefix)
            let b = Bmi1.BitFieldExtract(hash, uint16 m)
            if lz >= (m >>> 16) then b
            else 2u
        else
            if getPrefix hash m = prefix then zeroBit hash m
            else 2u
    #else
    let inline matchPrefixAndGetBit (hash: uint32) (prefix: uint32) (m: uint32) =
        if getPrefix hash m = prefix then zeroBit hash m
        else 2u
    #endif

    let inline compareMasks (l : Mask) (r : Mask) =
        #if NETCOREAPP3_0 && USE_INTRINSICS 
        if Bmi1.IsSupported then
            int (r &&& 0xFFu) - int (l &&& 0xFFu)
        else
            compare r l
        #else
        compare r l
        #endif


    let inline getMask (p0 : uint32) (p1 : uint32) =
        #if NETCOREAPP3_0 && USE_INTRINSICS 
        if Bmi1.IsSupported then
            let lz = Lzcnt.LeadingZeroCount(p0 ^^^ p1)
            (lz <<< 16) ||| 0x0100u ||| (31u - lz)
        else
            //lowestBitMask (p0 ^^^ p1) // little endian
            highestBitMask (p0 ^^^ p1) // big endian
        #else
        //lowestBitMask (p0 ^^^ p1) // little endian
        highestBitMask (p0 ^^^ p1) // big endian
        #endif

    let inline (==) (a: ^a) (b: ^a) =
        System.Object.ReferenceEquals(a, b)

    [<AllowNullLiteral>]
    type Linked<'K, 'V> =
        val mutable public Next: Linked<'K, 'V>
        val mutable public Key: 'K
        val mutable public Value: 'V

        new(k, v) = { Key = k; Value = v; Next = null }
        new(k, v, n) = { Key = k; Value = v; Next = n }

    module Linked =
    
        let rec addInPlaceUnsafe (cmp: EqualityComparer<'K>) (key: 'K) (value: 'V) (n: Linked<'K, 'V>) =
            if isNull n then
                Linked(key, value)
            elif cmp.Equals(n.Key, key) then
                n.Key <- key
                n.Value <- value
                n
            else
                n.Next <- addInPlaceUnsafe cmp key value n.Next
                n

        let rec add (cmp: EqualityComparer<'K>) (key: 'K) (value: 'V) (n: Linked<'K, 'V>) =
            if isNull n then
                Linked(key, value)
            elif cmp.Equals(n.Key, key) then
                Linked(key, value, n.Next)
            else
                Linked(n.Key, n.Value, add cmp key value n.Next)
               
        let rec alter (cmp: EqualityComparer<'K>) (key: 'K) (update: option<'V> -> option<'V>) (n: Linked<'K, 'V>) =
            if isNull n then
                match update None with
                | Some value -> 
                    Linked(key, value)
                | None ->
                    null
            elif cmp.Equals(n.Key, key) then
                match update (Some n.Value) with
                | Some value -> 
                    Linked(key, value, n.Next)
                | None -> 
                    n.Next
            else
                let next = alter cmp key update n.Next
                if next == n.Next then n
                else Linked(n.Key, n.Value, next)
               
        let rec alterV (cmp: EqualityComparer<'K>) (key: 'K) (update: voption<'V> -> voption<'V>) (n: Linked<'K, 'V>) =
            if isNull n then
                match update ValueNone with
                | ValueSome value -> 
                    Linked(key, value)
                | ValueNone ->
                    null
            elif cmp.Equals(n.Key, key) then
                match update (ValueSome n.Value) with
                | ValueSome value -> 
                    Linked(key, value, n.Next)
                | ValueNone -> 
                    n.Next
            else
                let next = alterV cmp key update n.Next
                if next == n.Next then n
                else Linked(n.Key, n.Value, next)
               
        let rec tryFind (cmp: EqualityComparer<'K>) (key: 'K) (n: Linked<'K, 'V>) =
            if isNull n then None
            elif cmp.Equals(n.Key, key) then Some n.Value
            else tryFind cmp key n.Next
            
        let rec tryFindV (cmp: EqualityComparer<'K>) (key: 'K) (n: Linked<'K, 'V>) =
            if isNull n then ValueNone
            elif cmp.Equals(n.Key, key) then ValueSome n.Value
            else tryFindV cmp key n.Next
            
        let rec containsKey (cmp: EqualityComparer<'K>) (key: 'K) (n: Linked<'K, 'V>) =
            if isNull n then false
            elif cmp.Equals(n.Key, key) then true
            else containsKey cmp key n.Next

        let destruct (n: Linked<'K, 'V>) =
            if isNull n then ValueNone
            else ValueSome(struct (n.Key, n.Value, n.Next))
            
        let rec remove (cmp: EqualityComparer<'K>) (key: 'K) (n: Linked<'K, 'V>) =
            if isNull n then
                null
            elif cmp.Equals(n.Key, key) then 
                n.Next
            else
                let rest = remove cmp key n.Next
                if rest == n.Next then n
                else Linked(n.Key, n.Value, rest)

        let rec tryRemove (cmp: EqualityComparer<'K>) (key: 'K) (n: Linked<'K, 'V>) =
            if isNull n then
                ValueNone
            elif cmp.Equals(n.Key, key) then 
                ValueSome (struct(n.Value, n.Next))
            else
                match tryRemove cmp key n.Next with
                | ValueSome (struct (value, rest)) ->
                    ValueSome(struct(value, Linked(n.Key, n.Value, rest)))
                | ValueNone ->
                    ValueNone

        let rec map (mapping: OptimizedClosures.FSharpFunc<'K, 'V, 'T>) (n: Linked<'K, 'V>) = 
            if isNull n then
                null
            else 
                let r = mapping.Invoke(n.Key, n.Value)
                Linked(n.Key, r, map mapping n.Next)

        let rec choose (mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>) (n: Linked<'K, 'V>) = 
            if isNull n then
                null
            else 
                match mapping.Invoke(n.Key, n.Value) with
                | Some r -> 
                    Linked(n.Key, r, choose mapping n.Next)
                | None -> 
                    choose mapping n.Next
    
        let rec chooseV (mapping: OptimizedClosures.FSharpFunc<'K, 'V, ValueOption<'T>>) (n: Linked<'K, 'V>) = 
            if isNull n then
                null
            else 
                match mapping.Invoke(n.Key, n.Value) with
                | ValueSome r -> 
                    Linked(n.Key, r, chooseV mapping n.Next)
                | ValueNone -> 
                    chooseV mapping n.Next
    
        let rec chooseV2 (mapping: OptimizedClosures.FSharpFunc<'K, 'V, struct(ValueOption<'T1> * ValueOption<'T2>)>) (n: Linked<'K, 'V>) = 
            if isNull n then
                struct(null, null)
            else 
                let struct (l, r) = mapping.Invoke(n.Key, n.Value)
                let struct (lr, rr) = chooseV2 mapping n.Next

                let left = 
                    match l with
                    | ValueSome l -> Linked(n.Key, l, lr)
                    | ValueNone -> lr
                let right =
                    match r with
                    | ValueSome r -> Linked(n.Key, r, rr)
                    | ValueNone -> rr
                struct(left, right)
    
    
        let rec filter (predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) (n: Linked<'K, 'V>) =
            if isNull n then
                null
            elif predicate.Invoke(n.Key, n.Value) then
                Linked(n.Key, n.Value, filter predicate n.Next)
            else
                filter predicate n.Next
    
        let rec exists (predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) (n: Linked<'K, 'V>) =
            if isNull n then 
                false
            elif predicate.Invoke(n.Key, n.Value) then
                true
            else
                exists predicate n.Next
                
        let rec forall (predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) (n: Linked<'K, 'V>) =
            if isNull n then 
                true
            elif not (predicate.Invoke(n.Key, n.Value)) then
                false
            else
                forall predicate n.Next

        let rec copyTo (index: ref<int>) (dst : ('K * 'V) array) (n: Linked<'K, 'V>) =
            if not (isNull n) then
                dst.[!index] <- n.Key, n.Value
                index := !index + 1
                copyTo index dst n.Next
    

    [<AbstractClass>]
    type HashMapNode<'K, 'V>() =
        abstract member Remove: EqualityComparer<'K> * uint32 * 'K -> HashMapNode<'K, 'V>
        abstract member TryRemove: EqualityComparer<'K> * uint32 * 'K -> ValueOption<struct ('V * HashMapNode<'K, 'V>)>

        abstract member Count : int

        abstract member AddInPlaceUnsafe: EqualityComparer<'K> * uint32 * 'K * 'V -> HashMapNode<'K, 'V>
        abstract member Add: EqualityComparer<'K> * uint32 * 'K * 'V -> HashMapNode<'K, 'V>
        abstract member Alter: EqualityComparer<'K> * uint32 * 'K * (option<'V> -> option<'V>) -> HashMapNode<'K, 'V>
        abstract member TryFind: EqualityComparer<'K> * uint32 * 'K -> option<'V>
        abstract member AlterV: EqualityComparer<'K> * uint32 * 'K * (voption<'V> -> voption<'V>) -> HashMapNode<'K, 'V>
        abstract member TryFindV: EqualityComparer<'K> * uint32 * 'K -> voption<'V>
        abstract member ContainsKey: EqualityComparer<'K> * uint32 * 'K -> bool
        abstract member IsEmpty: bool

        abstract member Map: mapping: OptimizedClosures.FSharpFunc<'K, 'V, 'T> -> HashMapNode<'K, 'T>
        abstract member Choose: mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>> -> HashMapNode<'K, 'T>
        abstract member ChooseV: mapping: OptimizedClosures.FSharpFunc<'K, 'V, ValueOption<'T>> -> HashMapNode<'K, 'T>
        abstract member ChooseV2: mapping: OptimizedClosures.FSharpFunc<'K, 'V, struct(ValueOption<'T1> * ValueOption<'T2>)> -> struct (HashMapNode<'K, 'T1> * HashMapNode<'K, 'T2>)
        abstract member Filter: mapping: OptimizedClosures.FSharpFunc<'K, 'V, bool> -> HashMapNode<'K, 'V>
        abstract member Iter: action: OptimizedClosures.FSharpFunc<'K, 'V, unit> -> unit
        abstract member Fold: acc: OptimizedClosures.FSharpFunc<'S, 'K, 'V, 'S> * seed : 'S -> 'S
        abstract member Exists: predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool> -> bool
        abstract member Forall: predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool> -> bool

        abstract member Accept: NodeVisitor<'K, 'V, 'R> -> 'R

        abstract member ToArray: ref<array<struct('K * 'V)>> * ref<int> -> unit

        abstract member CopyTo: dst: ('K * 'V) array * index : ref<int> -> unit

    and [<AbstractClass>] Leaf<'K, 'V>() =
        inherit HashMapNode<'K, 'V>()
        abstract member LHash : uint32
        abstract member LKey : 'K
        abstract member LValue : 'V
        abstract member LNext : Linked<'K, 'V>
        
        static member inline New(h: uint32, k: 'K, v: 'V, n: Linked<'K, 'V>) : HashMapNode<'K, 'V> = 
            if isNull n then new NoCollisionLeaf<'K, 'V>(Hash = h, Key = k, Value = v) :> HashMapNode<'K, 'V>
            else new CollisionLeaf<'K, 'V>(Hash = h, Key = k, Value = v, Next = n) :> HashMapNode<'K, 'V>
     
    and [<Sealed>] Empty<'K, 'V> private() =
        inherit HashMapNode<'K, 'V>()
        static let instance = Empty<'K, 'V>() :> HashMapNode<_,_>
        static member Instance = instance

        override x.Count = 0

        override x.ToArray(dst, o) =
            ()

        override x.Accept(v: NodeVisitor<_,_,_>) =
            v.VisitEmpty x

        override x.IsEmpty = true

        override x.TryFind(_cmp: EqualityComparer<'K>, _hash: uint32, _key: 'K) =
            None
            
        override x.TryFindV(_cmp: EqualityComparer<'K>, _hash: uint32, _key: 'K) =
            ValueNone

        override x.ContainsKey(_cmp: EqualityComparer<'K>, _hash: uint32, _key: 'K) =
            false

        override x.Remove(_cmp: EqualityComparer<'K>, _hash: uint32, _key: 'K) =
            x:> _
            
        override x.TryRemove(_cmp: EqualityComparer<'K>, _hash: uint32, _key: 'K) =
            ValueNone

        override x.AddInPlaceUnsafe(_cmp: EqualityComparer<'K>, hash: uint32, key: 'K, value: 'V) =
            NoCollisionLeaf<'K, 'V>.New(hash, key, value)

        override x.Add(_cmp: EqualityComparer<'K>, hash: uint32, key: 'K, value: 'V) =
            NoCollisionLeaf<'K, 'V>.New(hash, key, value)

        override x.Alter(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, update: option<'V> -> option<'V>) =
            match update None with
            | None -> x:> _
            | Some value ->
                NoCollisionLeaf<'K, 'V>.New(hash, key, value)
                
        override x.AlterV(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, update: voption<'V> -> voption<'V>) =
            match update ValueNone with
            | ValueNone -> x:> _
            | ValueSome value ->
                NoCollisionLeaf<'K, 'V>.New(hash, key, value)

        override x.Map(_mapping: OptimizedClosures.FSharpFunc<'K, 'V, 'T>) =
            Empty<'K, 'T>.Instance
            
        override x.Choose(_mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>) =
            Empty<'K, 'T>.Instance
            
        override x.ChooseV(_mapping: OptimizedClosures.FSharpFunc<'K, 'V, ValueOption<'T>>) =
            Empty<'K, 'T>.Instance
                 
        override x.ChooseV2(_mapping) =
            struct(Empty.Instance, Empty.Instance)
                          
        override x.Filter(_predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            Empty<'K, 'V>.Instance

        override x.Iter(_action: OptimizedClosures.FSharpFunc<'K, 'V, unit>) =
            ()
            
        override x.Fold(_acc: OptimizedClosures.FSharpFunc<'S, 'K, 'V, 'S>, seed : 'S) =
            seed

        override x.Exists(_predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            false

        override x.Forall(_predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            true

        override x.CopyTo(_dst : ('K * 'V) array, _index : ref<int>) =
            ()

    and [<Sealed>] CollisionLeaf<'K, 'V>() =
        inherit Leaf<'K, 'V>()

        [<DefaultValue>]
        val mutable public Next: Linked<'K, 'V>
        [<DefaultValue>]
        val mutable public Key: 'K
        [<DefaultValue>]
        val mutable public Value: 'V
        [<DefaultValue>]
        val mutable public Hash: uint32
  
        override x.Count =
            let mutable cnt = 1
            let mutable c = x.Next
            while not (isNull c) do
                c <- c.Next
                cnt <- cnt + 1
            cnt

        override x.LHash = x.Hash
        override x.LKey = x.Key
        override x.LValue = x.Value
        override x.LNext = x.Next

        override x.ToArray(dst, o) =
            if !o >= dst.Value.Length then System.Array.Resize(&dst.contents, !o * 2)
            dst.Value.[!o] <- struct(x.Key, x.Value)
            o := !o + 1
            
            let mutable n = x.Next
            while not (isNull n) do
                if !o >= dst.Value.Length then System.Array.Resize(&dst.contents, !o * 2)
                dst.Value.[!o] <- struct(n.Key, n.Value)
                o := !o + 1
                n <- n.Next

        member x.GetEntries() =
            let mutable arr = Array.zeroCreate 8
            arr.[0] <- struct(x.Key, x.Value)
            let mutable cnt = 1

            let mutable n = x.Next
            while not (isNull n) do
                if cnt >= arr.Length then System.Array.Resize(&arr, cnt * 2)
                arr.[cnt] <- struct(n.Key, n.Value)
                cnt <- cnt + 1
                n <- n.Next
            if cnt < arr.Length then System.Array.Resize(&arr, cnt)
            arr

        override x.Accept(v: NodeVisitor<_,_,_>) =
            v.VisitLeaf x

        override x.IsEmpty = false
        
        override x.TryFind(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =   
            if hash = x.Hash then
                if cmp.Equals(key, x.Key) then 
                    Some x.Value
                else
                    Linked.tryFind cmp key x.Next
            else
                None

        override x.TryFindV(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =   
            if hash = x.Hash then
                if cmp.Equals(key, x.Key) then 
                    ValueSome x.Value
                else
                    Linked.tryFindV cmp key x.Next
            else
                ValueNone

        override x.ContainsKey(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =   
            if hash = x.Hash then
                if cmp.Equals(key, x.Key) then 
                    true
                else
                    Linked.containsKey cmp key x.Next
            else
                false

        override x.Remove(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            if hash = x.Hash then
                if cmp.Equals(key, x.Key) then
                    match Linked.destruct x.Next with
                    | ValueSome (struct (k, v, rest)) ->
                        Leaf.New(hash, k, v, rest)
                    | ValueNone ->
                        Empty<'K, 'V>.Instance
                else
                    let next = Linked.remove cmp key x.Next
                    if next == x.Next then x :> _
                    else Leaf.New(x.Hash, x.Key, x.Value, Linked.remove cmp key x.Next)
            else
                x:> _

        override x.TryRemove(cmp: EqualityComparer<'K>, hash: uint32, key: 'K)         =
            if hash = x.Hash then
                if cmp.Equals(key, x.Key) then
                    match Linked.destruct x.Next with
                    | ValueSome (struct (k, v, rest)) ->
                        ValueSome(struct(x.Value, Leaf.New(hash, k, v, rest)))
                    | ValueNone ->
                        ValueSome(struct(x.Value, Empty.Instance))
                else
                    match Linked.tryRemove cmp key x.Next with
                    | ValueSome(struct(value, rest)) ->
                        ValueSome(
                            struct(
                                value,
                                Leaf.New(x.Hash, x.Key, x.Value, rest)
                            )
                        )
                    | ValueNone ->
                        ValueNone
            else
                ValueNone

        override x.AddInPlaceUnsafe(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, value: 'V) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    x.Key <- key
                    x.Value <- value
                    x:> _
                else
                    x.Next <- Linked.addInPlaceUnsafe cmp key value x.Next
                    x:> _
            else
                let n = NoCollisionLeaf<'K, 'V>.New(hash, key, value)
                Node.Join(hash, n, x.Hash, x)
                
        override x.Add(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, value: 'V) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    CollisionLeaf.New(x.Hash, key, value, x.Next)
                else
                    CollisionLeaf.New(x.Hash, x.Key, x.Value, Linked.add cmp key value x.Next)
            else
                let n = NoCollisionLeaf<'K, 'V>.New(hash, key, value)
                Node.Join(hash, n, x.Hash, x)

        override x.Alter(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, update: option<'V> -> option<'V>) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    match update (Some x.Value) with
                    | None ->
                        // remove
                        match Linked.destruct x.Next with
                        | ValueSome (struct (k, v, rest)) ->
                            Leaf.New(x.Hash, k, v, rest)
                        | ValueNone ->
                            Empty<'K, 'V>.Instance
                    | Some value ->
                        // update
                        CollisionLeaf.New(x.Hash, x.Key, value, x.Next) 
                else
                    // in linked?
                    let n = Linked.alter cmp key update x.Next
                    if n == x.Next then x:> _
                    else Leaf.New(x.Hash, x.Key, x.Value, n)
            else
                // other hash => not contained
                match update None with
                | None -> x:> _
                | Some value ->
                    // add
                    let n = NoCollisionLeaf<'K, 'V>.New(hash, key, value)
                    Node.Join(hash, n, x.Hash, x)

        override x.AlterV(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, update: voption<'V> -> voption<'V>) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    match update (ValueSome x.Value) with
                    | ValueNone ->
                        // remove
                        match Linked.destruct x.Next with
                        | ValueSome (struct (k, v, rest)) ->
                            Leaf.New(x.Hash, k, v, rest)
                        | ValueNone ->
                            Empty<'K, 'V>.Instance
                    | ValueSome value ->
                        // update
                        CollisionLeaf.New(x.Hash, x.Key, value, x.Next) 
                else
                    // in linked?
                    let n = Linked.alterV cmp key update x.Next
                    if n == x.Next then x:> _
                    else Leaf.New(x.Hash, x.Key, x.Value, n)
            else
                // other hash => not contained
                match update ValueNone with
                | ValueNone -> x:> _
                | ValueSome value ->
                    // add
                    let n = NoCollisionLeaf<'K, 'V>.New(hash, key, value)
                    Node.Join(hash, n, x.Hash, x)

        override x.Map(mapping: OptimizedClosures.FSharpFunc<'K, 'V, 'T>) =
            let t = mapping.Invoke(x.Key, x.Value)
            CollisionLeaf.New(x.Hash, x.Key, t, Linked.map mapping x.Next)
            
        override x.Choose(mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>) =
            match mapping.Invoke(x.Key, x.Value) with
            | Some v ->
                Leaf.New(x.Hash, x.Key, v, Linked.choose mapping x.Next)
            | None -> 
                let rest = Linked.choose mapping x.Next
                match Linked.destruct rest with
                | ValueSome (struct (key, value, rest)) ->
                    Leaf.New(x.Hash, key, value, rest)
                | ValueNone ->
                    Empty<'K, 'T>.Instance

        override x.ChooseV(mapping: OptimizedClosures.FSharpFunc<'K, 'V, ValueOption<'T>>) =
            match mapping.Invoke(x.Key, x.Value) with
            | ValueSome v ->
                Leaf.New(x.Hash, x.Key, v, Linked.chooseV mapping x.Next)
            | ValueNone -> 
                let rest = Linked.chooseV mapping x.Next
                match Linked.destruct rest with
                | ValueSome (struct (key, value, rest)) ->
                    Leaf.New(x.Hash, key, value, rest)
                | ValueNone ->
                    Empty<'K, 'T>.Instance

        override x.ChooseV2(mapping: OptimizedClosures.FSharpFunc<'K, 'V, struct (ValueOption<'T1> * ValueOption<'T2>)>) =
            let struct (l,r) = mapping.Invoke(x.Key, x.Value)
            let struct (ln, rn) = Linked.chooseV2 mapping x.Next
            let left = 
                match l with
                | ValueSome v -> Leaf.New(x.Hash, x.Key, v, ln) :> HashMapNode<_,_>
                | ValueNone -> 
                    match Linked.destruct ln with
                    | ValueSome (struct (key, value, rest)) ->
                        Leaf.New(x.Hash, key, value, rest)
                    | ValueNone ->
                        Empty<'K, 'T1>.Instance
            let right = 
                match r with
                | ValueSome v -> Leaf.New(x.Hash, x.Key, v, rn) :> HashMapNode<_,_>
                | ValueNone -> 
                    match Linked.destruct rn with
                    | ValueSome (struct (key, value, rest)) ->
                        Leaf.New(x.Hash, key, value, rest)
                    | ValueNone ->
                        Empty<'K, 'T2>.Instance
            struct (left, right)

        override x.Filter(predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            if predicate.Invoke(x.Key, x.Value) then
                Leaf.New(x.Hash, x.Key, x.Value, Linked.filter predicate x.Next)
            else
                let rest = Linked.filter predicate x.Next
                match Linked.destruct rest with
                | ValueSome (struct (key, value, rest)) ->
                    Leaf.New(x.Hash, key, value, rest)
                | ValueNone ->
                    Empty<'K, 'V>.Instance

        override x.Iter(action: OptimizedClosures.FSharpFunc<'K, 'V, unit>) =
            action.Invoke(x.Key, x.Value)
            let mutable n = x.Next
            while not (isNull n) do
                action.Invoke(n.Key, n.Value)
                n <- n.Next
                
        override x.Fold(acc: OptimizedClosures.FSharpFunc<'S, 'K, 'V, 'S>, seed : 'S) =
            let mutable res = acc.Invoke(seed, x.Key, x.Value)
            let mutable n = x.Next
            while not (isNull n) do
                res <- acc.Invoke(res, n.Key, n.Value)
                n <- n.Next
            res

        override x.Exists(predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            if predicate.Invoke(x.Key, x.Value) then true
            else Linked.exists predicate x.Next
                
        override x.Forall(predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            if predicate.Invoke(x.Key, x.Value) then Linked.forall predicate x.Next
            else false

        override x.CopyTo(dst : ('K * 'V) array, index : ref<int>) =
            dst.[!index] <- (x.Key, x.Value)
            index := !index + 1
            Linked.copyTo index dst x.Next
            
        static member New(h: uint32, k: 'K, v: 'V, n: Linked<'K, 'V>) : HashMapNode<'K, 'V> = 
            assert (not (isNull n))
            new CollisionLeaf<'K, 'V>(Hash = h, Key = k, Value = v, Next = n) :> HashMapNode<'K, 'V>
     
    and [<Sealed>] NoCollisionLeaf<'K, 'V>() =
        inherit Leaf<'K, 'V>()
        [<DefaultValue>]
        val mutable public Key: 'K
        [<DefaultValue>]
        val mutable public Value: 'V
        [<DefaultValue>]
        val mutable public Hash: uint32

        override x.Count = 1
        override x.LHash = x.Hash
        override x.LKey = x.Key
        override x.LValue = x.Value
        override x.LNext = null

        override x.ToArray(dst, o) =
            if !o >= dst.Value.Length then System.Array.Resize(&dst.contents, !o * 2)
            dst.Value.[!o] <- struct(x.Key, x.Value)
            o := !o + 1
        
        override x.IsEmpty = false
        
        override x.Accept(v: NodeVisitor<_,_,_>) =
            v.VisitNoCollision x

        override x.TryFind(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =   
            if hash = x.Hash && cmp.Equals(key, x.Key) then 
                Some x.Value
            else
                None

        override x.TryFindV(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =   
            if hash = x.Hash && cmp.Equals(key, x.Key) then 
                ValueSome x.Value
            else
                ValueNone

        override x.ContainsKey(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =   
            if hash = x.Hash && cmp.Equals(key, x.Key) then 
                true
            else
                false

        override x.Remove(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            if hash = x.Hash && cmp.Equals(key, x.Key) then
                Empty<'K, 'V>.Instance
            else
                x:> _

        override x.TryRemove(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            if hash = x.Hash && cmp.Equals(key, x.Key) then
                ValueSome (struct(x.Value, Empty<'K, 'V>.Instance))
            else
                ValueNone

        override x.AddInPlaceUnsafe(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, value: 'V) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    x.Key <- key
                    x.Value <- value
                    x:> _
                else
                    CollisionLeaf.New(x.Hash, x.Key, x.Value, Linked(key, value, null))
            else
                let n = NoCollisionLeaf.New(hash, key, value)
                Node.Join(hash, n, x.Hash, x)

        override x.Add(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, value: 'V) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    NoCollisionLeaf.New(x.Hash, key, value)
                else
                    CollisionLeaf.New(x.Hash, x.Key, x.Value, Linked.add cmp key value null)
            else
                let n = NoCollisionLeaf.New(hash, key, value)
                Node.Join(hash, n, x.Hash, x)
        
        override x.Alter(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, update: option<'V> -> option<'V>) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    match update (Some x.Value) with
                    | Some value -> 
                        NoCollisionLeaf.New(x.Hash, x.Key, value)
                    | None -> 
                        Empty.Instance
                else
                    match update None with
                    | None -> x:> _
                    | Some value ->
                        CollisionLeaf.New(x.Hash, x.Key, x.Value, Linked(key, value, null))
            else
                match update None with
                | None -> x:> _
                | Some value ->
                    let n = NoCollisionLeaf.New(hash, key, value)
                    Node.Join(hash, n, x.Hash, x)
           
        override x.AlterV(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, update: voption<'V> -> voption<'V>) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    match update (ValueSome x.Value) with
                    | ValueSome value -> 
                        NoCollisionLeaf.New(x.Hash, x.Key, value)
                    | ValueNone -> 
                        Empty.Instance
                else
                    match update ValueNone with
                    | ValueNone -> x:> _
                    | ValueSome value ->
                        CollisionLeaf.New(x.Hash, x.Key, x.Value, Linked(key, value, null))
            else
                match update ValueNone with
                | ValueNone -> x:> _
                | ValueSome value ->
                    let n = NoCollisionLeaf.New(hash, key, value)
                    Node.Join(hash, n, x.Hash, x)
           
        override x.Map(mapping: OptimizedClosures.FSharpFunc<'K, 'V, 'T>) =
            let t = mapping.Invoke(x.Key, x.Value)
            NoCollisionLeaf.New(x.Hash, x.Key, t)
               
        override x.Choose(mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>) =
            match mapping.Invoke(x.Key, x.Value) with
            | Some v ->
                NoCollisionLeaf.New(x.Hash, x.Key, v)
            | None ->
                Empty<'K, 'T>.Instance
                
        override x.ChooseV(mapping: OptimizedClosures.FSharpFunc<'K, 'V, ValueOption<'T>>) =
            match mapping.Invoke(x.Key, x.Value) with
            | ValueSome v ->
                NoCollisionLeaf.New(x.Hash, x.Key, v)
            | ValueNone ->
                Empty<'K, 'T>.Instance
 
        override x.ChooseV2(mapping: OptimizedClosures.FSharpFunc<'K, 'V, struct (ValueOption<'T1> * ValueOption<'T2>)>) =
            let struct (l,r) = mapping.Invoke(x.Key, x.Value)         
            let l = match l with | ValueSome v -> NoCollisionLeaf.New(x.Hash, x.Key, v) :> HashMapNode<_,_> | _ -> Empty.Instance
            let r = match r with | ValueSome v -> NoCollisionLeaf.New(x.Hash, x.Key, v) :> HashMapNode<_,_> | _ -> Empty.Instance
            struct (l, r)

        override x.Filter(predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            if predicate.Invoke(x.Key, x.Value) then
                NoCollisionLeaf.New(x.Hash, x.Key, x.Value)
            else
                Empty<'K, 'V>.Instance
 
        override x.Iter(action: OptimizedClosures.FSharpFunc<'K, 'V, unit>) =
            action.Invoke(x.Key, x.Value)
            
        override x.Fold(acc: OptimizedClosures.FSharpFunc<'S, 'K, 'V, 'S>, seed : 'S) =
            acc.Invoke(seed, x.Key, x.Value)

        override x.Exists(predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            predicate.Invoke(x.Key, x.Value)
                
        override x.Forall(predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            predicate.Invoke(x.Key, x.Value)

        override x.CopyTo(dst : ('K * 'V) array, index : ref<int>) =
            dst.[!index] <- (x.Key, x.Value)
            index := !index + 1

        static member New(h : uint32, k : 'K, v : 'V) : HashMapNode<'K, 'V> =
            new NoCollisionLeaf<'K, 'V>(Hash = h, Key = k, Value = v) :> HashMapNode<'K, 'V>

    and [<Sealed>] Node<'K, 'V>() =
        inherit HashMapNode<'K, 'V>()
        [<DefaultValue>]
        val mutable public Prefix: uint32
        [<DefaultValue>]
        val mutable public Mask: Mask
        [<DefaultValue>]
        val mutable public Left: HashMapNode<'K, 'V>
        [<DefaultValue>]
        val mutable public Right: HashMapNode<'K, 'V>
        [<DefaultValue>]
        val mutable public _Count: int

        override x.Count = x._Count

        static member Join (p0 : uint32, t0 : HashMapNode<'K, 'V>, p1 : uint32, t1 : HashMapNode<'K, 'V>) : HashMapNode<'K,'V>=
            let m = getMask p0 p1
            if zeroBit p0 m = 0u then Node.New(getPrefix p0 m, m, t0, t1)
            else Node.New(getPrefix p0 m, m, t1, t0)

        static member Create(p: uint32, m: Mask, l: HashMapNode<'K, 'V>, r: HashMapNode<'K, 'V>) =
            if r.IsEmpty then l
            elif l.IsEmpty then r
            else Node.New(p, m, l, r)

        override x.ToArray(dst, o) =
            x.Left.ToArray(dst, o)
            x.Right.ToArray(dst, o)

        override x.IsEmpty = false
        
        override x.Accept(v: NodeVisitor<_,_,_>) =
            v.VisitNode x

        override x.TryFind(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            #if OPTIMISTIC 
            let m = zeroBit hash x.Mask
            if m = 0u then x.Left.TryFind(cmp, hash, key)
            else x.Right.TryFind(cmp, hash, key)
            #else
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then x.Left.TryFind(cmp, hash, key)
            elif m = 1u then x.Right.TryFind(cmp, hash, key)
            else None
            #endif

        override x.TryFindV(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            #if OPTIMISTIC 
            let m = zeroBit hash x.Mask
            if m = 0u then x.Left.TryFindV(cmp, hash, key)
            else x.Right.TryFindV(cmp, hash, key)
            #else
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then x.Left.TryFindV(cmp, hash, key)
            elif m = 1u then x.Right.TryFindV(cmp, hash, key)
            else ValueNone
            #endif
            
        override x.ContainsKey(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            #if OPTIMISTIC 
            let m = zeroBit hash x.Mask
            if m = 0u then x.Left.ContainsKey(cmp, hash, key)
            else x.Right.ContainsKey(cmp, hash, key)
            #else
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then x.Left.ContainsKey(cmp, hash, key)
            elif m = 1u then x.Right.ContainsKey(cmp, hash, key)
            else false
            #endif

        override x.Remove(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                let l = x.Left.Remove(cmp, hash, key)
                if l == x.Left then x :> _
                else Node.Create(x.Prefix, x.Mask, l, x.Right)
            elif m = 1u then
                let r = x.Right.Remove(cmp, hash, key)
                if r == x.Right then x :> _
                else Node.Create(x.Prefix, x.Mask, x.Left, r)
            else
                x:> _

        override x.TryRemove(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                match x.Left.TryRemove(cmp, hash, key) with
                | ValueSome (struct(value, ll)) ->
                    ValueSome (struct(value, Node.Create(x.Prefix, x.Mask, ll, x.Right)))
                | ValueNone ->
                    ValueNone
            elif m = 1u then
                match x.Right.TryRemove(cmp, hash, key) with
                | ValueSome (struct(value, rr)) ->
                    ValueSome (struct(value, Node.Create(x.Prefix, x.Mask, x.Left, rr)))
                | ValueNone ->
                    ValueNone
            else
                ValueNone

        override x.AddInPlaceUnsafe(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, value: 'V) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                x.Left <- x.Left.AddInPlaceUnsafe(cmp, hash, key, value)
                x._Count <- x.Left.Count + x.Right.Count
                x:> HashMapNode<_,_>
            elif m = 1u then 
                x.Right <- x.Right.AddInPlaceUnsafe(cmp, hash, key, value)
                x._Count <- x.Left.Count + x.Right.Count
                x:> HashMapNode<_,_>
            else
                let n = NoCollisionLeaf.New(hash, key, value)
                Node.Join(x.Prefix, x, hash, n)

        override x.Add(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, value: 'V) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                Node.New(x.Prefix, x.Mask, x.Left.Add(cmp, hash, key, value), x.Right)
            elif m = 1u then 
                Node.New(x.Prefix, x.Mask, x.Left, x.Right.Add(cmp, hash, key, value))
            else
                let n = NoCollisionLeaf.New(hash, key, value)
                Node.Join(x.Prefix, x, hash, n)

        override x.Alter(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, update: option<'V> -> option<'V>) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                let ll = x.Left.Alter(cmp, hash, key, update)
                if ll == x.Left then x:> _
                else Node.New(x.Prefix, x.Mask, ll, x.Right)
            elif m = 1u then
                let rr = x.Right.Alter(cmp, hash, key, update)
                if rr == x.Right then x:> _
                else Node.New(x.Prefix, x.Mask, x.Left, rr)
            else
                match update None with
                | None -> x:> _
                | Some value ->
                    let n = NoCollisionLeaf.New(hash, key, value)
                    Node.Join(x.Prefix, x, hash, n)
                    
        override x.AlterV(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, update: voption<'V> -> voption<'V>) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                let ll = x.Left.AlterV(cmp, hash, key, update)
                if ll == x.Left then x:> _
                else Node.New(x.Prefix, x.Mask, ll, x.Right)
            elif m = 1u then
                let rr = x.Right.AlterV(cmp, hash, key, update)
                if rr == x.Right then x:> _
                else Node.New(x.Prefix, x.Mask, x.Left, rr)
            else
                match update ValueNone with
                | ValueNone -> x:> _
                | ValueSome value ->
                    let n = NoCollisionLeaf.New(hash, key, value)
                    Node.Join(x.Prefix, x, hash, n)
                    
        override x.Map(mapping: OptimizedClosures.FSharpFunc<'K, 'V, 'T>) =
            Node.New(x.Prefix, x.Mask, x.Left.Map(mapping), x.Right.Map(mapping))
  
        override x.Choose(mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>) =
            Node.Create(x.Prefix, x.Mask, x.Left.Choose(mapping), x.Right.Choose(mapping))
            
        override x.ChooseV(mapping: OptimizedClosures.FSharpFunc<'K, 'V, ValueOption<'T>>) =
            Node.Create(x.Prefix, x.Mask, x.Left.ChooseV(mapping), x.Right.ChooseV(mapping))
      
        override x.ChooseV2(mapping: OptimizedClosures.FSharpFunc<'K, 'V, struct(ValueOption<'T1> * ValueOption<'T2>)>) =
            let struct (la, lb) = x.Left.ChooseV2(mapping)
            let struct (ra, rb) = x.Right.ChooseV2(mapping)

            struct (
                Node.Create(x.Prefix, x.Mask, la, ra),
                Node.Create(x.Prefix, x.Mask, lb, rb)
            )
      
        override x.Filter(predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            Node.Create(x.Prefix, x.Mask, x.Left.Filter(predicate), x.Right.Filter(predicate))
            
        override x.Iter(action: OptimizedClosures.FSharpFunc<'K, 'V, unit>) =
            x.Left.Iter(action)
            x.Right.Iter(action)

        override x.Fold(acc: OptimizedClosures.FSharpFunc<'S, 'K, 'V, 'S>, seed : 'S) =
            let s = x.Left.Fold(acc, seed)
            x.Right.Fold(acc, s)
            

        override x.Exists(predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            x.Left.Exists predicate || x.Right.Exists predicate
                
        override x.Forall(predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            x.Left.Forall predicate && x.Right.Forall predicate

        override x.CopyTo(dst : ('K * 'V) array, index : ref<int>) =
            x.Left.CopyTo(dst, index)
            x.Right.CopyTo(dst, index)

        static member New(p: uint32, m: Mask, l: HashMapNode<'K, 'V>, r: HashMapNode<'K, 'V>) : HashMapNode<'K, 'V> = 
            new Node<'K, 'V>(Prefix = p, Mask = m, Left = l, Right = r, _Count = l.Count + r.Count) :> _

    and [<AbstractClass>] NodeVisitor<'K, 'V, 'R>() =
        abstract member VisitNode: Node<'K, 'V> -> 'R
        abstract member VisitLeaf: CollisionLeaf<'K, 'V> -> 'R
        abstract member VisitNoCollision: NoCollisionLeaf<'K, 'V> -> 'R
        abstract member VisitEmpty: Empty<'K, 'V> -> 'R
        
    and [<AbstractClass>] NodeVisitor2<'K, 'V1, 'V2, 'R>() =
        abstract member VisitNN     : Node<'K, 'V1> * Node<'K, 'V2> -> 'R

        abstract member VisitNL     : Node<'K, 'V1> * Leaf<'K, 'V2> -> 'R
        abstract member VisitLN     : Leaf<'K, 'V1> * Node<'K, 'V2> -> 'R
        abstract member VisitLL     : Leaf<'K, 'V1> * Leaf<'K, 'V2> -> 'R

        abstract member VisitAE     : HashMapNode<'K, 'V1> * Empty<'K, 'V2> -> 'R
        abstract member VisitEA     : Empty<'K, 'V1> * HashMapNode<'K, 'V2> -> 'R
        abstract member VisitEE     : Empty<'K, 'V1> * Empty<'K, 'V2> -> 'R

    type Visit2Visitor<'K, 'V1, 'V2, 'R>(real : NodeVisitor2<'K, 'V1, 'V2, 'R>, node : HashMapNode<'K, 'V2>) =
        inherit NodeVisitor<'K, 'V1, 'R>()

        override x.VisitLeaf l = 
            node.Accept {
                new NodeVisitor<'K, 'V2, 'R>() with
                    member x.VisitLeaf r = real.VisitLL(l, r)
                    member x.VisitNode r = real.VisitLN(l, r)
                    member x.VisitNoCollision r = real.VisitLL(l, r)
                    member x.VisitEmpty r = real.VisitAE(l, r)
            }
            
        override x.VisitNode l = 
            node.Accept {
                new NodeVisitor<'K, 'V2, 'R>() with
                    member x.VisitLeaf r = real.VisitNL(l, r)
                    member x.VisitNode r = real.VisitNN(l, r)
                    member x.VisitNoCollision r = real.VisitNL(l, r)
                    member x.VisitEmpty r = real.VisitAE(l, r)
            }
            
        override x.VisitNoCollision l = 
            node.Accept {
                new NodeVisitor<'K, 'V2, 'R>() with
                    member x.VisitLeaf r = real.VisitLL(l, r)
                    member x.VisitNode r = real.VisitLN(l, r)
                    member x.VisitNoCollision r = real.VisitLL(l, r)
                    member x.VisitEmpty r = real.VisitAE(l, r)
            }
            
        override x.VisitEmpty l = 
            node.Accept {
                new NodeVisitor<'K, 'V2, 'R>() with
                    member x.VisitLeaf r = real.VisitEA(l, r)
                    member x.VisitNode r = real.VisitEA(l, r)
                    member x.VisitNoCollision r = real.VisitEA(l, r)
                    member x.VisitEmpty r = real.VisitEE(l, r)
            }

    module Visit2 = 
        let visit (v : NodeVisitor2<'K, 'V1, 'V2, 'R>) (l : HashMapNode<'K, 'V1>) (r : HashMapNode<'K, 'V2>) =
            l.Accept (Visit2Visitor(v, r))

[<Struct>]
type HashMapOkasaki<'K, 'V> internal(cmp: EqualityComparer<'K>, root: HashMapNode<'K, 'V>) =

    static member Empty = HashMapOkasaki<'K, 'V>(EqualityComparer<'K>.Default, Empty.Instance)

    member x.Count = root.Count
    member x.IsEmpty = root.IsEmpty

    member internal x.Root = root
    
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Single(key: 'K, value : 'V) =  
        let cmp = EqualityComparer<'K>.Default
        HashMapOkasaki(cmp, NoCollisionLeaf.New(uint32 (cmp.GetHashCode key), key, value))
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfSeq(elements: seq<'K * 'V>) =  
        let cmp = EqualityComparer<'K>.Default
        let mutable r = HashMapOkasakiImplementation.Empty<'K, 'V>.Instance 
        for (k, v) in elements do
            let hash = cmp.GetHashCode k |> uint32
            r <- r.AddInPlaceUnsafe(cmp, hash, k, v)
        HashMapOkasaki<'K, 'V>(cmp, r)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfList(elements: list<'K * 'V>) =  
        let cmp = EqualityComparer<'K>.Default
        let mutable r = HashMapOkasakiImplementation.Empty<'K, 'V>.Instance 
        for (k, v) in elements do
            let hash = cmp.GetHashCode k |> uint32
            r <- r.AddInPlaceUnsafe(cmp, hash, k, v)
        HashMapOkasaki<'K, 'V>(cmp, r)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfListUnoptimized(elements: list<'K * 'V>) =  
        let cmp = EqualityComparer<'K>.Default
        let mutable r = HashMapOkasakiImplementation.Empty<'K, 'V>.Instance 
        for (k, v) in elements do
            let hash = cmp.GetHashCode k |> uint32
            r <- r.Add(cmp, hash, k, v)
        HashMapOkasaki<'K, 'V>(cmp, r)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfArray(elements: array<'K * 'V>) =  
        let cmp = EqualityComparer<'K>.Default
        let mutable r = HashMapOkasakiImplementation.Empty<'K, 'V>.Instance 
        for (k, v) in elements do
            let hash = cmp.GetHashCode k |> uint32
            r <- r.AddInPlaceUnsafe(cmp, hash, k, v)
        HashMapOkasaki<'K, 'V>(cmp, r)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Add(key: 'K, value: 'V) =
        let hash = cmp.GetHashCode key |> uint32
        let newRoot = root.Add(cmp, hash, key, value)
        HashMapOkasaki(cmp, newRoot)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Remove(key: 'K) =
        let hash = cmp.GetHashCode key |> uint32
        let newRoot = root.Remove(cmp, hash, key)
        HashMapOkasaki(cmp, newRoot)
         
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.TryRemove(key: 'K) =
        let hash = cmp.GetHashCode key |> uint32
        match root.TryRemove(cmp, hash, key) with
        | ValueSome (struct(value, newRoot)) ->
            Some (value, HashMapOkasaki(cmp, newRoot))
        | ValueNone ->
            None
         
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.TryFind(key: 'K) =
        let hash = cmp.GetHashCode key |> uint32
        root.TryFind(cmp, hash, key)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.TryFindV(key: 'K) =
        let hash = cmp.GetHashCode key |> uint32
        root.TryFindV(cmp, hash, key)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ContainsKey(key: 'K) =
        let hash = cmp.GetHashCode key |> uint32
        root.ContainsKey(cmp, hash, key)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Alter(key: 'K, update: option<'V> -> option<'V>) =
        let hash = cmp.GetHashCode key |> uint32
        let newRoot = root.Alter(cmp, hash, key, update)
        HashMapOkasaki(cmp, newRoot)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.AlterV(key: 'K, update: voption<'V> -> voption<'V>) =
        let hash = cmp.GetHashCode key |> uint32
        let newRoot = root.AlterV(cmp, hash, key, update)
        HashMapOkasaki(cmp, newRoot)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Map(mapping: 'K -> 'V -> 'T) =
        let mapping = OptimizedClosures.FSharpFunc<'K, 'V, 'T>.Adapt mapping
        let newRoot = root.Map(mapping)
        HashMapOkasaki(cmp, newRoot)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Choose(mapping: 'K -> 'V -> option<'T>) =
        let mapping = OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>.Adapt mapping
        let newRoot = root.Choose(mapping)
        HashMapOkasaki(cmp, newRoot)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Filter(predicate: 'K -> 'V -> bool) =
        let predicate = OptimizedClosures.FSharpFunc<'K, 'V, bool>.Adapt predicate
        let newRoot = root.Filter(predicate)
        HashMapOkasaki(cmp, newRoot)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Iter(action: 'K -> 'V -> unit) =
        let action = OptimizedClosures.FSharpFunc<'K, 'V, unit>.Adapt action
        root.Iter(action)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Fold(acc: 'S -> 'K -> 'V -> 'S, seed : 'S) =
        let acc = OptimizedClosures.FSharpFunc<'S, 'K, 'V, 'S>.Adapt acc
        root.Fold(acc, seed)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Exists(predicate: 'K -> 'V -> bool) =
        let predicate = OptimizedClosures.FSharpFunc<'K, 'V, bool>.Adapt predicate
        root.Exists predicate
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Forall(predicate: 'K -> 'V -> bool) =
        let predicate = OptimizedClosures.FSharpFunc<'K, 'V, bool>.Adapt predicate
        root.Forall predicate
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member inline x.ToSeq() =
        x :> seq<_>
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToArray() =
        let arr = Array.zeroCreate root.Count
        let index = ref 0
        root.CopyTo(arr, index)
        arr
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ComputeDelta(l : HashMapOkasaki<'K, 'V>, r : HashMapOkasaki<'K, 'V>, add : 'K -> 'V -> 'OP, update : 'K -> 'V -> 'V -> ValueOption<'OP>, remove : 'K -> 'V -> 'OP) =   
        let add = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(add)
        let remove = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(remove)
        let update = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(update)

        
        let len = ref 0
        let arr = ref (Array.zeroCreate 4)
        let cmp = EqualityComparer<'K>.Default

        let result = 
            let cnt = ()
            (l.Root, r.Root) ||> Visit2.visit {
                new NodeVisitor2<'K, 'V, 'V, HashMapNode<'K, 'OP>>() with

                    member x.VisitEE(_, _) = Empty<'K, 'OP>.Instance
                    member x.VisitEA(_, r) = r.Map(add)
                    member x.VisitAE(l, _) = l.Map(remove)

                    member x.VisitLL(l, r) = 
                        if l == r then
                            Empty<'K, 'OP>.Instance
                        else
                            len := 0
                            if l.LHash = r.LHash then
                                let mutable r = r :> HashMapNode<_,_>
                                let mutable res = Empty<'K, 'OP>.Instance
                                let hash = l.LHash
                        
                                l.ToArray(arr, len)
                                for i in 0 .. !len - 1 do
                                    let struct (k, lv) = arr.Value.[i]
                                    match r.TryRemove(cmp, hash, k) with
                                    | ValueSome (rv, rest) ->
                                        r <- rest
                                        match update.Invoke(k, lv, rv) with
                                        | ValueSome op ->
                                            res <- res.AddInPlaceUnsafe(cmp, hash, k, op)
                                        | ValueNone ->
                                            ()
                                    | ValueNone ->
                                        res <- res.AddInPlaceUnsafe(cmp, hash, k, remove.Invoke(k, lv))

                                len := 0
                                r.ToArray(arr, len)
                                for i in 0 .. !len - 1 do
                                    let struct (k, rv) = arr.Value.[i]
                                    res <- res.AddInPlaceUnsafe(cmp, hash, k, add.Invoke(k, rv))
                        
                                res
                            else
                                let mutable res = l.Map(remove)
                                r.ToArray(arr, len)
                                for i in 0 .. !len - 1 do
                                    let struct (k, rv) = arr.Value.[i]
                                    res <- res.AddInPlaceUnsafe(cmp, r.LHash, k, add.Invoke(k, rv))
                                res

                    member x.VisitLN(l, r) =
                        let b = matchPrefixAndGetBit l.LHash r.Prefix r.Mask
                        if b = 0u then
                            Node.Create(r.Prefix, r.Mask, Visit2.visit x l r.Left, r.Right.Map(add))
                        elif b = 1u then
                            Node.Create(r.Prefix, r.Mask, r.Left.Map(add), Visit2.visit x l r.Right)
                        else
                            Node.Join(l.LHash, l.Map(remove), r.Prefix, r.Map(add))

                    member x.VisitNL(l, r) =
                        let b = matchPrefixAndGetBit r.LHash l.Prefix l.Mask
                        if b = 0u then
                            Node.Create(l.Prefix, l.Mask, Visit2.visit x l.Left r, l.Right.Map(remove))
                        elif b = 1u then
                            Node.Create(l.Prefix, l.Mask, l.Left.Map(remove), Visit2.visit x l.Right r)
                        else
                            Node.Join(l.Prefix, l.Map(remove), r.LHash, r.Map(add))

                    member x.VisitNN(l, r) = 
                        if l == r then
                            Empty<'K, 'OP>.Instance
                        else
                            let cc = compareMasks l.Mask r.Mask
                            if cc = 0 then
                                let l' = (l.Left, r.Left) ||> Visit2.visit x
                                let r' = (l.Right, r.Right) ||> Visit2.visit x
                                Node.Create(l.Prefix, l.Mask, l', r')
                            elif cc > 0 then
                                let lr = matchPrefixAndGetBit l.Prefix r.Prefix r.Mask
                                if lr = 0u then
                                    Node.Create(r.Prefix, r.Mask, Visit2.visit x l r.Left, r.Right.Map(add))
                                elif lr = 1u then
                                    Node.Create(r.Prefix, r.Mask, r.Left.Map(add), Visit2.visit x l r.Right)
                                else
                                    Node.Join(l.Prefix, l.Map(remove), r.Prefix, r.Map(add))
                            else
                                let rl = matchPrefixAndGetBit r.Prefix l.Prefix l.Mask
                            
                                if rl = 0u then
                                    Node.Create(l.Prefix, l.Mask, Visit2.visit x l.Left r, l.Right.Map(remove))
                                elif rl = 1u then
                                    Node.Create(l.Prefix, l.Mask, l.Left.Map(remove), Visit2.visit x l.Right r)
                                else
                                    Node.Join(l.Prefix, l.Map(remove), r.Prefix, r.Map(add))
                                    
            }

        HashMapOkasaki(cmp, result)
 
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member UnionWith(l : HashMapOkasaki<'K, 'V>, r : HashMapOkasaki<'K, 'V>, resolve : 'K -> 'V -> 'V -> 'V) =   
        let update = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(resolve)

        let len = ref 0
        let arr = ref (Array.zeroCreate 4)
        let cmp = EqualityComparer<'K>.Default

        let result = 
            let cnt = ()
            (l.Root, r.Root) ||> Visit2.visit {
                new NodeVisitor2<'K, 'V, 'V, HashMapNode<'K, 'V>>() with

                    member x.VisitEE(_, _) = Empty<'K, 'V>.Instance
                    member x.VisitEA(_, r) = r
                    member x.VisitAE(l, _) = l

                    member x.VisitLL(l, r) = 
                        len := 0
                        if l.LHash = r.LHash then
                            let mutable r = r :> HashMapNode<_,_>
                            let mutable res = Empty<'K, 'V>.Instance
                            let hash = l.LHash
                    
                            l.ToArray(arr, len)
                            for i in 0 .. !len - 1 do
                                let struct (k, lv) = arr.Value.[i]
                                match r.TryRemove(cmp, hash, k) with
                                | ValueSome (rv, rest) ->
                                    r <- rest
                                    let op = update.Invoke(k, lv, rv)
                                    res <- res.AddInPlaceUnsafe(cmp, hash, k, op)
                                | ValueNone ->
                                    res <- res.AddInPlaceUnsafe(cmp, hash, k, lv)

                            len := 0
                            r.ToArray(arr, len)
                            for i in 0 .. !len - 1 do
                                let struct (k, rv) = arr.Value.[i]
                                res <- res.AddInPlaceUnsafe(cmp, hash, k, rv)
                    
                            res
                        else
                            Node.Join(l.LHash, l, r.LHash, r)
                         

                    member x.VisitLN(l, r) =
                        let b = matchPrefixAndGetBit l.LHash r.Prefix r.Mask
                        if b = 0u then
                            Node.Create(r.Prefix, r.Mask, Visit2.visit x l r.Left, r.Right)
                        elif b = 1u then
                            Node.Create(r.Prefix, r.Mask, r.Left, Visit2.visit x l r.Right)
                        else
                            Node.Join(l.LHash, l, r.Prefix, r)

                    member x.VisitNL(l, r) =
                        let b = matchPrefixAndGetBit r.LHash l.Prefix l.Mask
                        if b = 0u then
                            Node.Create(l.Prefix, l.Mask, Visit2.visit x l.Left r, l.Right)
                        elif b = 1u then
                            Node.Create(l.Prefix, l.Mask, l.Left, Visit2.visit x l.Right r)
                        else
                            Node.Join(l.Prefix, l, r.LHash, r)

                    member x.VisitNN(l, r) = 
                        let cc = compareMasks l.Mask r.Mask
                        if cc = 0 then
                            let l' = (l.Left, r.Left) ||> Visit2.visit x
                            let r' = (l.Right, r.Right) ||> Visit2.visit x
                            Node.Create(l.Prefix, l.Mask, l', r')
                        elif cc > 0 then
                            let lr = matchPrefixAndGetBit l.Prefix r.Prefix r.Mask
                            if lr = 0u then
                                Node.Create(r.Prefix, r.Mask, Visit2.visit x l r.Left, r.Right)
                            elif lr = 1u then
                                Node.Create(r.Prefix, r.Mask, r.Left, Visit2.visit x l r.Right)
                            else
                                Node.Join(l.Prefix, l, r.Prefix, r)
                        else
                            let rl = matchPrefixAndGetBit r.Prefix l.Prefix l.Mask
                        
                            if rl = 0u then
                                Node.Create(l.Prefix, l.Mask, Visit2.visit x l.Left r, l.Right)
                            elif rl = 1u then
                                Node.Create(l.Prefix, l.Mask, l.Left, Visit2.visit x l.Right r)
                            else
                                Node.Join(l.Prefix, l, r.Prefix, r)
                                
            }

        HashMapOkasaki(cmp, result)
  
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Union(l : HashMapOkasaki<'K, 'V>, r : HashMapOkasaki<'K, 'V>) =   

        let len = ref 0
        let arr = ref (Array.zeroCreate 4)
        let cmp = EqualityComparer<'K>.Default

        let result = 
            (l.Root, r.Root) ||> Visit2.visit {
                new NodeVisitor2<'K, 'V, 'V, HashMapNode<'K, 'V>>() with

                    member x.VisitEE(_, _) = Empty<'K, 'V>.Instance
                    member x.VisitEA(_, r) = r
                    member x.VisitAE(l, _) = l

                    member x.VisitLL(l, r) = 
                        if l == r then
                            r :> HashMapNode<_,_>
                        else
                            len := 0
                            if l.LHash = r.LHash then
                                let mutable r = r :> HashMapNode<_,_>
                                let mutable res = Empty<'K, 'V>.Instance
                                let hash = l.LHash
                
                                l.ToArray(arr, len)
                                for i in 0 .. !len - 1 do
                                    let struct (k, lv) = arr.Value.[i]
                                    match r.TryRemove(cmp, hash, k) with
                                    | ValueSome (rv, rest) ->
                                        r <- rest
                                        res <- res.AddInPlaceUnsafe(cmp, hash, k, rv)
                                    | ValueNone ->
                                        res <- res.AddInPlaceUnsafe(cmp, hash, k, lv)

                                len := 0
                                r.ToArray(arr, len)
                                for i in 0 .. !len - 1 do
                                    let struct (k, rv) = arr.Value.[i]
                                    res <- res.AddInPlaceUnsafe(cmp, hash, k, rv)
                
                                res
                            else
                                Node.Join(l.LHash, l, r.LHash, r)
                     

                    member x.VisitLN(l, r) =
                        let b = matchPrefixAndGetBit l.LHash r.Prefix r.Mask
                        if b = 0u then
                            Node.Create(r.Prefix, r.Mask, Visit2.visit x l r.Left, r.Right)
                        elif b = 1u then
                            Node.Create(r.Prefix, r.Mask, r.Left, Visit2.visit x l r.Right)
                        else
                            Node.Join(l.LHash, l, r.Prefix, r)

                    member x.VisitNL(l, r) =
                        let b = matchPrefixAndGetBit r.LHash l.Prefix l.Mask
                        if b = 0u then
                            Node.Create(l.Prefix, l.Mask, Visit2.visit x l.Left r, l.Right)
                        elif b = 1u then
                            Node.Create(l.Prefix, l.Mask, l.Left, Visit2.visit x l.Right r)
                        else
                            Node.Join(l.Prefix, l, r.LHash, r)

                    member x.VisitNN(l, r) = 
                        if l == r then 
                            r :> HashMapNode<_,_>
                        else
                            let cc = compareMasks l.Mask r.Mask
                            if cc = 0 then
                                let l' = (l.Left, r.Left) ||> Visit2.visit x
                                let r' = (l.Right, r.Right) ||> Visit2.visit x
                                Node.Create(l.Prefix, l.Mask, l', r')
                            elif cc > 0 then
                                let lr = matchPrefixAndGetBit l.Prefix r.Prefix r.Mask
                                if lr = 0u then
                                    Node.Create(r.Prefix, r.Mask, Visit2.visit x l r.Left, r.Right)
                                elif lr = 1u then
                                    Node.Create(r.Prefix, r.Mask, r.Left, Visit2.visit x l r.Right)
                                else
                                    Node.Join(l.Prefix, l, r.Prefix, r)
                            else
                                let rl = matchPrefixAndGetBit r.Prefix l.Prefix l.Mask
                    
                                if rl = 0u then
                                    Node.Create(l.Prefix, l.Mask, Visit2.visit x l.Left r, l.Right)
                                elif rl = 1u then
                                    Node.Create(l.Prefix, l.Mask, l.Left, Visit2.visit x l.Right r)
                                else
                                    Node.Join(l.Prefix, l, r.Prefix, r)
                            
            }

        HashMapOkasaki(cmp, result)
  

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ApplyDelta(state : HashMapOkasaki<'K, 'V>, delta : HashMapOkasaki<'K, 'D>, apply : 'K -> voption<'V> -> 'D -> struct(voption<'V> * voption<'D>)) =   
        let apply = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(apply)

        let onlyDelta = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(fun k d -> apply.Invoke(k, ValueNone, d))
    
        let len = ref 0
        let arr1 = ref (Array.zeroCreate 4)
        let arr2 = ref (Array.zeroCreate 4)
        let cmp = EqualityComparer<'K>.Default

        let struct(result, delta) = 
            let cnt = ()
            (state.Root, delta.Root) ||> Visit2.visit {
                new NodeVisitor2<'K, 'V, 'D, struct(HashMapNode<'K, 'V> * HashMapNode<'K, 'D>)>() with

                    member x.VisitEE(_, _) = 
                        struct (Empty.Instance, Empty.Instance)

                    member x.VisitEA(_, r) =    
                        r.ChooseV2 onlyDelta

                    member x.VisitAE(l, _) = 
                        struct(l, Empty.Instance)

                    member x.VisitLL(state, delta) = 
                        len := 0
                        if state.LHash = delta.LHash then
                            let mutable delta = delta :> HashMapNode<_,_>
                            let mutable resState = Empty<'K, 'V>.Instance
                            let mutable resDelta = Empty<'K, 'D>.Instance
                            let hash = state.LHash
                    
                            state.ToArray(arr1, len)
                            for i in 0 .. !len - 1 do
                                let struct (k, value) = arr1.Value.[i]
                                match delta.TryRemove(cmp, hash, k) with
                                | ValueSome (dd, rest) ->
                                    delta <- rest
                                    let struct (s, d) = apply.Invoke(k, ValueSome value, dd)

                                    match s with
                                    | ValueSome v -> resState <- resState.AddInPlaceUnsafe(cmp, hash, k, v)
                                    | ValueNone -> ()

                                    match d with
                                    | ValueSome v -> resDelta <- resDelta.AddInPlaceUnsafe(cmp, hash, k, v)
                                    | ValueNone -> ()

                                | ValueNone ->
                                    resState <- resState.AddInPlaceUnsafe(cmp, hash, k, value)

                            len := 0
                            delta.ToArray(arr2, len)
                            for i in 0 .. !len - 1 do
                                let struct (k, rv) = arr2.Value.[i]
                                let struct (s, d) = onlyDelta.Invoke(k, rv)
                                match s with
                                | ValueSome v -> resState <- resState.AddInPlaceUnsafe(cmp, hash, k, v)
                                | ValueNone -> ()
                                match d with
                                | ValueSome v -> resDelta <- resDelta.AddInPlaceUnsafe(cmp, hash, k, v)
                                | ValueNone -> ()
                    
                            struct(resState, resDelta)
                        else
                            let struct (ds, dd) = delta.ChooseV2(onlyDelta)
                            struct (
                                Node.Join(state.LHash, state, delta.LHash, ds),
                                dd
                            )

                    member x.VisitLN(state, delta) =
                        let b = matchPrefixAndGetBit state.LHash delta.Prefix delta.Mask
                        if b = 0u then
                            let struct (ls, ld) = Visit2.visit x state delta.Left
                            let struct (rs, rd) = delta.Right.ChooseV2(onlyDelta)
                            struct(
                                Node.Create(delta.Prefix, delta.Mask, ls, rs),
                                Node.Create(delta.Prefix, delta.Mask, ld, rd)
                            )
                        elif b = 1u then
                            let struct (ls, ld) = delta.Left.ChooseV2(onlyDelta)
                            let struct (rs, rd) = Visit2.visit x state delta.Right
                            struct(
                                Node.Create(delta.Prefix, delta.Mask, ls, rs),
                                Node.Create(delta.Prefix, delta.Mask, ld, rd)
                            )
                        else
                            let struct (ds, dd) = delta.ChooseV2(onlyDelta)
                            struct(
                                Node.Join(state.LHash, state, delta.Prefix, ds),
                                dd
                            )

                    member x.VisitNL(l, r) =
                        let b = matchPrefixAndGetBit r.LHash l.Prefix l.Mask
                        if b = 0u then
                            let struct (ls, ld) = Visit2.visit x l.Left r
                            struct (
                                Node.Create(l.Prefix, l.Mask, ls, l.Right),
                                ld
                            )
                        elif b = 1u then
                            let struct (rs, rd) = Visit2.visit x l.Right r
                            struct (
                                Node.Create(l.Prefix, l.Mask, l.Left, rs),
                                rd
                            )
                        else
                            let struct (rs, rd) = r.ChooseV2(onlyDelta)
                            struct (
                                Node.Join(l.Prefix, l, r.LHash, rs),
                                rd
                            )

                    member x.VisitNN(l, r) = 
                        let cc = compareMasks l.Mask r.Mask
                        if cc = 0 then
                            let struct (ls, ld) = (l.Left, r.Left) ||> Visit2.visit x
                            let struct (rs, rd) = (l.Right, r.Right) ||> Visit2.visit x
                            struct (
                                Node.Create(l.Prefix, l.Mask, ls, rs),
                                Node.Create(l.Prefix, l.Mask, ld, rd)
                            )
                        elif cc > 0 then
                            let lr = matchPrefixAndGetBit l.Prefix r.Prefix r.Mask
                            if lr = 0u then
                                let struct (ls, ld) = Visit2.visit x l r.Left
                                let struct (rs, rd) = r.Right.ChooseV2(onlyDelta)
                                struct (
                                    Node.Create(r.Prefix, r.Mask, ls, rs),
                                    Node.Create(r.Prefix, r.Mask, ld, rd)
                                )
                            elif lr = 1u then
                                let struct (ls, ld) = r.Left.ChooseV2(onlyDelta)
                                let struct (rs, rd) = Visit2.visit x l r.Right
                                struct (
                                    Node.Create(r.Prefix, r.Mask, ls, rs),
                                    Node.Create(r.Prefix, r.Mask, ld, rd)
                                )
                            else
                                let struct (rs, rd) = r.ChooseV2 onlyDelta
                                struct (
                                    Node.Join(l.Prefix, l, r.Prefix, rs),
                                    rd
                                )
                        else
                            let rl = matchPrefixAndGetBit r.Prefix l.Prefix l.Mask
                        
                            if rl = 0u then
                                let struct (ls, ld) = Visit2.visit x l.Left r
                                struct (
                                    Node.Create(l.Prefix, l.Mask, ls, l.Right),
                                    ld
                                )
                            elif rl = 1u then
                                let struct (rs, rd) = Visit2.visit x l.Right r
                                struct (
                                    Node.Create(l.Prefix, l.Mask, l.Left, rs),
                                    rd
                                )
                            else
                                let struct (rs, rd) = r.ChooseV2 onlyDelta
                                struct (
                                    Node.Join(l.Prefix, l, r.Prefix, rs),
                                    rd
                                )
                                
            }

        HashMapOkasaki(cmp, result), HashMapOkasaki(cmp, delta)
 

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToList() =
        let arr = Array.zeroCreate root.Count
        let index = ref 0
        root.CopyTo(arr, index)
        let mutable res = []
        for i in 1 .. arr.Length do
            let i = arr.Length - i
            res <- arr.[i] :: res
        res

    interface System.Collections.IEnumerable with 
        member x.GetEnumerator() = new HashMapOkasakiEnumerator<_,_>(root) :> _
        
    interface System.Collections.Generic.IEnumerable<'K * 'V> with 
        member x.GetEnumerator() = new HashMapOkasakiEnumerator<_,_>(root) :> _

and internal HashMapOkasakiEnumerator<'K, 'V>(root: HashMapNode<'K, 'V>) =
    let mutable stack = [root]
    let mutable linked: Linked<'K, 'V> = null
    let mutable current = Unchecked.defaultof<'K * 'V>

    member x.MoveNext() =
        if isNull linked then
            match stack with
            | (:? Empty<'K, 'V>) :: rest ->
                stack <- rest 
                x.MoveNext()
            | (:? NoCollisionLeaf<'K, 'V> as l) :: rest ->
                stack <- rest
                current <- l.Key, l.Value
                true
            | (:? CollisionLeaf<'K, 'V> as l) :: rest -> 
                stack <- rest
                current <- l.Key, l.Value
                linked <- l.Next
                true
            | (:? Node<'K, 'V> as n) :: rest ->
                stack <- n.Left:: n.Right:: rest
                x.MoveNext()
            | _ ->
                false
        else
            current <- (linked.Key, linked.Value)
            linked <- linked.Next
            true
    
    member x.Current = current

    member x.Reset() =
        stack <- [root]
        linked <- null
        current <- Unchecked.defaultof<_>

    member x.Dispose() =
        stack <- []
        linked <- null
        current <- Unchecked.defaultof<_>

    interface System.Collections.IEnumerator with
        member x.MoveNext() = x.MoveNext()
        member x.Current = x.Current:> obj
        member x.Reset() = x.Reset()
        
    interface System.Collections.Generic.IEnumerator<'K * 'V> with
        member x.Dispose() = x.Dispose()
        member x.Current = x.Current

module HashMapOkasaki =

    /// The empty map.
    [<GeneralizableValue>]
    let empty<'K, 'V> = HashMapOkasaki<'K, 'V>.Empty

    /// The number of elements in the map `O(1)`
    let inline count (map: HashMapOkasaki<'K, 'V>) = map.Count
    
    /// Is the map empty? `O(1)`
    let inline isEmpty (map: HashMapOkasaki<'K, 'V>) = map.IsEmpty

    /// Creates a map with a single entry.
    /// `O(1)`
    let inline single (key: 'K) (value: 'V) =
        HashMapOkasaki<'K,'V>.Single(key, value)

    /// Creates a map with all entries from the seq.
    /// `O(N * log N)`
    let inline ofSeq (seq: seq<'K * 'V>) =
        HashMapOkasaki<'K, 'V>.OfSeq seq

    /// Creates a map with all entries from the map.
    /// `O(N * log N)`
    let inline ofMap (map: Map<'K, 'V>) = 
        map |> Map.toSeq |> ofSeq

    /// Creates a map with all entries from the list.
    /// `O(N * log N)`
    let inline ofList (list: list<'K * 'V>) = 
        HashMapOkasaki<'K, 'V>.OfList list

    /// Creates a map with all entries from the array.
    /// `O(N * log N)`
    let inline ofArray (arr: array<'K * 'V>) = 
        HashMapOkasaki<'K, 'V>.OfArray arr

    /// Creates a map with all entries from the list.
    /// `O(N * log N)`
    let inline ofListUnoptimized (list: list<'K * 'V>) = 
        HashMapOkasaki<'K, 'V>.OfListUnoptimized list

    /// Creates a seq holding all tuples contained in the map.
    /// `O(N)`
    let inline toSeq (map: HashMapOkasaki<'K, 'V>) = 
        map.ToSeq()

    /// Creates a list holding all tuples contained in the map.
    /// `O(N)`
    let inline toList (map: HashMapOkasaki<'K, 'V>) = 
        map.ToList()

    /// Creates an array holding all tuples contained in the map.
    /// `O(N)`
    let inline toArray (map: HashMapOkasaki<'K, 'V>) = 
        map.ToArray()

    /// Creates a Map holding all entries contained in the HashMap.
    /// `O(N)`
    let inline toMap (map: HashMapOkasaki<'K, 'V>) =
        map.ToSeq() |> Map.ofSeq

    /// Adds or updates the entry for the given key. `O(log N)`
    let inline add (key: 'K) (value: 'V) (map: HashMapOkasaki<'K, 'V>) =
        map.Add(key, value)

    /// Removes the entry for the given key. `O(log N)`
    let inline remove (key: 'K) (map: HashMapOkasaki<'K, 'V>) =
        map.Remove(key)
 
    /// Tries to remove the entry for the given key from the map and returns its value and the rest of the map.
    /// `O(log N)`       
    let inline tryRemove (key: 'K) (map: HashMapOkasaki<'K, 'V>) =
        map.TryRemove(key)

    /// Tries to find the value for the given key.
    /// `O(log N)`
    let inline tryFind (key: 'K) (map: HashMapOkasaki<'K, 'V>) =
        map.TryFind(key)

    /// Finds the value for the given key and raises KeyNotFoundException on failure.
    /// `O(log N)`
    let inline find (key: 'K) (map: HashMapOkasaki<'K, 'V>) =
        match map.TryFindV key with
        | ValueSome v -> v
        | ValueNone -> raise <| KeyNotFoundException()

    /// Tests if an entry for the given key exists. `O(log N)`
    let inline containsKey (key: 'K) (map: HashMapOkasaki<'K, 'V>) =
        map.ContainsKey(key)

    /// Adds, deletes or updates the entry for the given key.
    /// The update functions gets the optional old value and may optionally return
    /// A new value (or None for deleting the entry).
    /// `O(log N)`
    let inline alter (key: 'K) (update: option<'V> -> option<'V>) (map: HashMapOkasaki<'K, 'V>) =
        map.Alter(key, update)
    
    /// Creates a new map (with the same keys) by applying the given function to all entries.
    /// `O(N)`
    let inline map (mapping: 'K -> 'V -> 'T) (map: HashMapOkasaki<'K, 'V>) =
        map.Map mapping
    
    /// Creates a new map (with the same keys) by applying the given function to all entries.
    /// `O(N)`
    let inline choose (mapping: 'K -> 'V -> option<'T>) (map: HashMapOkasaki<'K, 'V>) =
        map.Choose mapping
    
    /// Creates a new map (with the same keys) that contains all entries for which predicate was true.
    /// `O(N)`
    let inline filter (predicate: 'K -> 'V -> bool) (map: HashMapOkasaki<'K, 'V>) =
        map.Filter predicate

    /// Applies the iter function to all entries of the map.
    /// `O(N)`
    let inline iter (iter: 'K -> 'V -> unit) (map: HashMapOkasaki<'K, 'V>) =
        map.Iter iter

    /// Folds over all entries of the map.
    /// Note that the order for elements is undefined.
    /// `O(N)`
    let inline fold (folder: 'State -> 'K -> 'V -> 'State) (seed: 'State) (map: HashMapOkasaki<'K, 'V>) =
        map.Fold(folder, seed)
        
    /// Tests whether an entry making the predicate true exists.
    /// `O(N)`
    let inline exists (predicate: 'K -> 'V -> bool) (map: HashMapOkasaki<'K, 'V>) =
        map.Exists(predicate)

    /// Tests whether all entries fulfil the given predicate.
    /// `O(N)`
    let inline forall (predicate: 'K -> 'V -> bool) (map: HashMapOkasaki<'K, 'V>) =
        map.Forall(predicate)

    /// Creates a new map containing all elements from l and r.
    /// The resolve function is used to resolve conflicts.
    /// `O(N + M)`
    let inline unionWith (resolve : 'K -> 'V -> 'V -> 'V) (l : HashMapOkasaki<'K, 'V>) (r : HashMapOkasaki<'K, 'V>) =
        let resolve = OptimizedClosures.FSharpFunc<'K, 'V, 'V, 'V>.Adapt resolve
        HashMapOkasaki<'K, 'V>.UnionWith(l, r, fun k l r -> resolve.Invoke(k, l, r))
    
    /// Creates a new map containing all elements from l and r.
    /// Colliding entries are taken from r.
    /// `O(N + M)`        
    let inline union (l : HashMapOkasaki<'K, 'V>) (r : HashMapOkasaki<'K, 'V>) =
        HashMapOkasaki<'K, 'V>.Union(l, r)



    ///// Creates a HashSet holding all keys from the map.
    ///// `O(N)`
    //let inline keys (map: HashMap<'K, 'V>) = map.GetKeys()

    ///// Creates a new map by applying the mapping function to all entries.
    ///// The respective option-arguments are some whenever the left/right map has an entry for the current key.
    ///// Note that one of the options will always be some.
    ///// `O(N + M)`
    //let inline map2 (mapping: 'K -> option<'V> -> option<'V2> -> 'V3) (l: HashMap<'K, 'V>) (r: HashMap<'K, 'V2>) =
    //    l.Map2(r, mapping)

    ///// Creates a new map by applying the mapping function to all entries.
    ///// The respective option-arguments are some whenever the left/right map has an entry for the current key.
    ///// Note that one of the options will always be some.
    ///// `O(N + M)`
    //let inline choose2 (mapping: 'K -> option<'V1> -> option<'V2> -> option<'V3>) (l: HashMap<'K, 'V1>) (r: HashMap<'K, 'V2>) =
    //    l.Choose2(r, mapping)


    let inline computeDelta (l : HashMapOkasaki<'K, 'V>) (r : HashMapOkasaki<'K, 'V>) =
        let inline add _k v = Set v
        let inline remove _k _v = Remove
        let inline update _l o n =
            if Unchecked.equals o n then ValueNone
            else ValueSome (Set n)

        HashMapOkasaki<'K, 'V>.ComputeDelta(l, r, add, update, remove)

    let inline applyDelta (l : HashMapOkasaki<'K, 'V>) (r : HashMapOkasaki<'K, ElementOperation<'V>>) =
        let inline apply _ o n =
            match n with
            | Remove ->
                match o with
                | ValueSome _ -> struct (ValueNone, ValueSome Remove)
                | ValueNone -> struct (ValueNone, ValueNone)
            | Set v ->
                match o with
                | ValueSome o ->
                    if Unchecked.equals o v then struct (ValueSome o, ValueNone)
                    else struct(ValueSome v, ValueSome (Set v))
                | ValueNone ->
                    struct(ValueSome v, ValueSome (Set v))

        HashMapOkasaki<'K, 'V>.ApplyDelta(l, r, apply)



