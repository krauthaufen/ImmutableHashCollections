namespace ImmutableHashCollections

open System.Collections
open System.Collections.Generic
open System.Runtime.CompilerServices
#if NETCOREAPP3_0 && USE_INTRINSICS
open System.Runtime.Intrinsics.X86
open System.Runtime.InteropServices
#endif

[<AutoOpen>]
module internal HashMapOkasakiImplementation = 

    #if NETCOREAPP3_0 && USE_INTRINSICS
    type Mask = uint32
    //[<Struct; StructLayout(LayoutKind.Explicit, Size = 4)>]
    //type Mask =
    //    [<FieldOffset(0)>]
    //    val mutable public BitMask : uint16
    //    [<FieldOffset(2)>]
    //    val mutable public LeadingZeros : uint16
    //    new(lz,b) = { BitMask = b; LeadingZeros = lz }
    #else
    type Mask = uint32
    #endif

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
        k
        #else
        k &&& ~~~((m <<< 1) - 1u)
        #endif

    #if NETCOREAPP3_0 && USE_INTRINSICS
    let inline zeroBit (k: uint32) (m: Mask) =
        Bmi1.BitFieldExtract(k, uint16 m)
    #else
    let inline zeroBit (k: uint32) (m: uint32) =
        if (k &&& m) <> 0u then 1u else 0u
    #endif
        
    #if NETCOREAPP3_0 && USE_INTRINSICS 
    let inline matchPrefixAndGetBit (hash: uint32) (prefix: uint32) (m: Mask) =
        let lz = Lzcnt.LeadingZeroCount (hash ^^^ prefix)
        let b = Bmi1.BitFieldExtract(hash, uint16 m)
        if lz >= (m >>> 16) then b
        else 2u
    #else
    let inline matchPrefixAndGetBit (hash: uint32) (prefix: uint32) (m: uint32) =
        if getPrefix hash m = prefix then zeroBit hash m
        else 2u
    #endif

    let inline getMask (p0 : uint32) (p1 : uint32) =
        #if NETCOREAPP3_0 && USE_INTRINSICS 
        let lz = Lzcnt.LeadingZeroCount(p0 ^^^ p1) // |> uint16
        //Mask(lz, 0x100us ||| (31us - lz))
        (lz <<< 16) ||| 0x100u ||| (31u - lz)
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
    
        let rec addInPlaceUnsafe (cmp: EqualityComparer<'K>) (cnt: ref<int>) (key: 'K) (value: 'V) (n: Linked<'K, 'V>) =
            if isNull n then
                cnt:= !cnt + 1
                Linked(key, value)
            elif cmp.Equals(n.Key, key) then
                n.Key <- key
                n.Value <- value
                n
            else
                n.Next <- addInPlaceUnsafe cmp cnt key value n.Next
                n

        let rec add (cmp: EqualityComparer<'K>) (cnt: ref<int>) (key: 'K) (value: 'V) (n: Linked<'K, 'V>) =
            if isNull n then
                cnt:= !cnt + 1
                Linked(key, value)
            elif cmp.Equals(n.Key, key) then
                Linked(key, value, n.Next)
            else
                Linked(n.Key, n.Value, add cmp cnt key value n.Next)
               
        let rec alter (cmp: EqualityComparer<'K>) (cnt: ref<int>) (key: 'K) (update: option<'V> -> option<'V>) (n: Linked<'K, 'V>) =
            if isNull n then
                match update None with
                | Some value -> 
                    cnt:= !cnt + 1
                    Linked(key, value)
                | None ->
                    null
            elif cmp.Equals(n.Key, key) then
                match update (Some n.Value) with
                | Some value -> 
                    Linked(key, value, n.Next)
                | None -> 
                    cnt:= !cnt - 1
                    n.Next
            else
                Linked(n.Key, n.Value, alter cmp cnt key update n.Next)
               
        let rec tryFind (cmp: EqualityComparer<'K>) (key: 'K) (n: Linked<'K, 'V>) =
            if isNull n then None
            elif cmp.Equals(n.Key, key) then Some n.Value
            else tryFind cmp key n.Next
            
        let rec containsKey (cmp: EqualityComparer<'K>) (key: 'K) (n: Linked<'K, 'V>) =
            if isNull n then false
            elif cmp.Equals(n.Key, key) then true
            else containsKey cmp key n.Next

        let destruct (n: Linked<'K, 'V>) =
            if isNull n then ValueNone
            else ValueSome(struct (n.Key, n.Value, n.Next))
            
        let rec remove (cmp: EqualityComparer<'K>) (cnt: ref<int>) (key: 'K) (n: Linked<'K, 'V>) =
            if isNull n then
                null
            elif cmp.Equals(n.Key, key) then 
                cnt:= !cnt - 1
                n.Next
            else
                let rest = remove cmp cnt key n.Next
                if rest == n.Next then n
                else Linked(n.Key, n.Value, rest)

        let rec tryRemove (cmp: EqualityComparer<'K>) (cnt: ref<int>) (key: 'K) (n: Linked<'K, 'V>) =
            if isNull n then
                ValueNone
            elif cmp.Equals(n.Key, key) then 
                cnt:= !cnt - 1
                ValueSome (struct(n.Value, n.Next))
            else
                match tryRemove cmp cnt key n.Next with
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

        let rec choose (cnt: ref<int>) (mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>) (n: Linked<'K, 'V>) = 
            if isNull n then
                null
            else 
                match mapping.Invoke(n.Key, n.Value) with
                | Some r -> 
                    cnt:= !cnt + 1
                    Linked(n.Key, r, choose cnt mapping n.Next)
                | None -> 
                    choose cnt mapping n.Next
    
        let rec filter (cnt: ref<int>) (predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) (n: Linked<'K, 'V>) =
            if isNull n then
                null
            elif predicate.Invoke(n.Key, n.Value) then
                cnt:= !cnt + 1
                Linked(n.Key, n.Value, filter cnt predicate n.Next)
            else
                filter cnt predicate n.Next
    
        let rec copyTo (index: ref<int>) (dst : ('K * 'V) array) (n: Linked<'K, 'V>) =
            if not (isNull n) then
                dst.[!index] <- n.Key, n.Value
                index := !index + 1
                copyTo index dst n.Next
    
    [<AbstractClass>]
    type AbstractNode<'K, 'V>() =
        abstract member Remove: EqualityComparer<'K> * ref<int> * uint32 * 'K -> AbstractNode<'K, 'V>
        abstract member TryRemove: EqualityComparer<'K> * ref<int> * uint32 * 'K -> ValueOption<struct ('V * AbstractNode<'K, 'V>)>

        abstract member AddInPlaceUnsafe: EqualityComparer<'K> * ref<int> * uint32 * 'K * 'V -> AbstractNode<'K, 'V>
        abstract member Add: EqualityComparer<'K> * ref<int> * uint32 * 'K * 'V -> AbstractNode<'K, 'V>
        abstract member Alter: EqualityComparer<'K> * ref<int> * uint32 * 'K * (option<'V> -> option<'V>) -> AbstractNode<'K, 'V>
        abstract member TryFind: EqualityComparer<'K> * uint32 * 'K -> option<'V>
        abstract member ContainsKey: EqualityComparer<'K> * uint32 * 'K -> bool
        abstract member IsEmpty: bool

        abstract member Map: mapping: OptimizedClosures.FSharpFunc<'K, 'V, 'T> -> AbstractNode<'K, 'T>
        abstract member Choose: cnt: ref<int> * mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>> -> AbstractNode<'K, 'T>
        abstract member Filter: cnt: ref<int> * mapping: OptimizedClosures.FSharpFunc<'K, 'V, bool> -> AbstractNode<'K, 'V>

        abstract member Accept: NodeVisitor<'K, 'V, 'R> -> 'R

        abstract member CopyTo: dst: ('K * 'V) array * index : ref<int> -> unit

    and [<AbstractClass>] NodeVisitor<'K, 'V, 'R>() =
        abstract member VisitNode: Node<'K, 'V> -> 'R
        abstract member VisitLeaf: Leaf<'K, 'V> -> 'R
        abstract member VisitNoCollision: NoCollisionLeaf<'K, 'V> -> 'R
        abstract member VisitEmpty: Empty<'K, 'V> -> 'R

    and [<Sealed>] Empty<'K, 'V> private() =
        inherit AbstractNode<'K, 'V>()
        static let instance = Empty<'K, 'V>() :> AbstractNode<_,_>
        static member Instance = instance

        override x.Accept(v: NodeVisitor<_,_,_>) =
            v.VisitEmpty x

        override x.IsEmpty = true

        override x.TryFind(_cmp: EqualityComparer<'K>, _hash: uint32, _key: 'K) =
            None

        override x.ContainsKey(_cmp: EqualityComparer<'K>, _hash: uint32, _key: 'K) =
            false

        override x.Remove(_cmp: EqualityComparer<'K>, _cnt: ref<int>, _hash: uint32, _key: 'K) =
            x:> _
            
        override x.TryRemove(_cmp: EqualityComparer<'K>, _cnt: ref<int>, _hash: uint32, _key: 'K) =
            ValueNone

        override x.AddInPlaceUnsafe(_cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K, value: 'V) =
            cnt:= !cnt + 1
            #if USE_NO_COLLISION
            NoCollisionLeaf<'K, 'V>(hash, key, value) :> _
            #else
            Leaf(hash, key, value, null) :> _
            #endif
        override x.Add(_cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K, value: 'V) =
            cnt:= !cnt + 1
            #if USE_NO_COLLISION
            NoCollisionLeaf<'K, 'V>(hash, key, value) :> _
            #else
            Leaf(hash, key, value, null) :> _
            #endif
        override x.Alter(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K, update: option<'V> -> option<'V>) =
            match update None with
            | None -> x:> _
            | Some value ->
                cnt:= !cnt + 1
                #if USE_NO_COLLISION
                NoCollisionLeaf<'K, 'V>(hash, key, value) :> _
                #else
                Leaf(hash, key, value, null) :> _
                #endif

        override x.Map(_mapping: OptimizedClosures.FSharpFunc<'K, 'V, 'T>) =
            Empty<'K, 'T>.Instance
            
        override x.Choose(_cnt: ref<int>, _mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>) =
            Empty<'K, 'T>.Instance
            
        override x.Filter(_cnt: ref<int>, _predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            Empty<'K, 'V>.Instance

        override x.CopyTo(_dst : ('K * 'V) array, _index : ref<int>) =
            ()

    and [<Sealed>] Leaf<'K, 'V> =
        inherit AbstractNode<'K, 'V>
        val mutable public Next: Linked<'K, 'V>
        val mutable public Key: 'K
        val mutable public Value: 'V
        val mutable public Hash: uint32
        
        static member inline Create(hash: uint32, key: 'K, value: 'V, next: Linked<'K, 'V>) =
            #if USE_NO_COLLISION
            if isNull next then NoCollisionLeaf(hash, key, value) :> AbstractNode<'K, 'V>
            else Leaf(hash, key, value, next) :> AbstractNode<'K, 'V>
            #else
            Leaf(hash, key, value, next) :> AbstractNode<'K, 'V>
            #endif
            
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
        override x.ContainsKey(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =   
            if hash = x.Hash then
                if cmp.Equals(key, x.Key) then 
                    true
                else
                    Linked.containsKey cmp key x.Next
            else
                false

        override x.Remove(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K) =
            if hash = x.Hash then
                if cmp.Equals(key, x.Key) then
                    cnt:= !cnt - 1
                    match Linked.destruct x.Next with
                    | ValueSome (struct (k, v, rest)) ->
                        Leaf.Create(hash, k, v, rest)
                    | ValueNone ->
                        Empty<'K, 'V>.Instance
                else
                    Leaf.Create(x.Hash, x.Key, x.Value, Linked.remove cmp cnt key x.Next)
            else
                x:> _

        override x.TryRemove(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K)         =
            if hash = x.Hash then
                if cmp.Equals(key, x.Key) then
                    cnt:= !cnt - 1
                    match Linked.destruct x.Next with
                    | ValueSome (struct (k, v, rest)) ->
                        ValueSome(struct(x.Value, Leaf.Create(hash, k, v, rest)))
                    | ValueNone ->
                        ValueSome(struct(x.Value, Empty.Instance))
                else
                    match Linked.tryRemove cmp cnt key x.Next with
                    | ValueSome(struct(value, rest)) ->
                        ValueSome(
                            struct(
                                value,
                                Leaf.Create(x.Hash, x.Key, x.Value, rest)
                            )
                        )
                    | ValueNone ->
                        ValueNone
            else
                ValueNone

        override x.AddInPlaceUnsafe(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K, value: 'V) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    x.Key <- key
                    x.Value <- value
                    x:> _
                else
                    x.Next <- Linked.addInPlaceUnsafe cmp cnt key value x.Next
                    x:> _
            else
                cnt:= !cnt + 1
                #if USE_NO_COLLISION
                let n = NoCollisionLeaf<'K, 'V>(hash, key, value)
                #else
                let n = Leaf(hash, key, value, null)
                #endif
                Node.Join(hash, n, x.Hash, x)
                
        override x.Add(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K, value: 'V) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    Leaf<'K, 'V>(x.Hash, key, value, x.Next) :> _
                else
                    Leaf<'K, 'V>(x.Hash, x.Key, x.Value, Linked.add cmp cnt key value x.Next) :> _
            else
                cnt:= !cnt + 1
                #if USE_NO_COLLISION
                let n = NoCollisionLeaf<'K, 'V>(hash, key, value)
                #else
                let n = Leaf(hash, key, value, null)
                #endif
                Node.Join(hash, n, x.Hash, x)

        override x.Alter(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K, update: option<'V> -> option<'V>) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    match update (Some x.Value) with
                    | None ->
                        // remove
                        cnt:= !cnt - 1
                        match Linked.destruct x.Next with
                        | ValueSome (struct (k, v, rest)) ->
                            Leaf.Create(x.Hash, k, v, rest)
                        | ValueNone ->
                            Empty<'K, 'V>.Instance
                    | Some value ->
                        // update
                        Leaf(x.Hash, x.Key, value, x.Next) :> _
                else
                    // in linked?
                    let n = Linked.alter cmp cnt key update x.Next
                    if n == x.Next then x:> _
                    else Leaf(x.Hash, x.Key, x.Value, n) :> _
            else
                // other hash => not contained
                match update None with
                | None -> x:> _
                | Some value ->
                    // add
                    cnt:= !cnt + 1
                    #if USE_NO_COLLISION
                    let n = NoCollisionLeaf<'K, 'V>(hash, key, value)
                    #else
                    let n = Leaf(hash, key, value, null)
                    #endif
                    Node.Join(hash, n, x.Hash, x)

        override x.Map(mapping: OptimizedClosures.FSharpFunc<'K, 'V, 'T>) =
            let t = mapping.Invoke(x.Key, x.Value)
            Leaf(x.Hash, x.Key, t, Linked.map mapping x.Next) :> _
            
        override x.Choose(cnt: ref<int>, mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>) =
            match mapping.Invoke(x.Key, x.Value) with
            | Some v ->
                cnt:= !cnt + 1
                Leaf(x.Hash, x.Key, v, Linked.choose cnt mapping x.Next) :> _
            | None -> 
                let rest = Linked.choose cnt mapping x.Next
                match Linked.destruct rest with
                | ValueSome (struct (key, value, rest)) ->
                    #if USE_NO_COLLISION
                    if isNull rest then NoCollisionLeaf(x.Hash, key, value) :> _
                    else Leaf(x.Hash, key, value, rest) :> _
                    #else
                    Leaf(x.Hash, key, value, rest) :> _
                    #endif
                | ValueNone ->
                    Empty<'K, 'T>.Instance

        override x.Filter(cnt: ref<int>, predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            if predicate.Invoke(x.Key, x.Value) then
                cnt:= !cnt + 1
                Leaf(x.Hash, x.Key, x.Value, Linked.filter cnt predicate x.Next) :> _
            else
                let rest = Linked.filter cnt predicate x.Next
                match Linked.destruct rest with
                | ValueSome (struct (key, value, rest)) ->
                    #if USE_NO_COLLISION
                    if isNull rest then NoCollisionLeaf(x.Hash, key, value) :> _
                    else Leaf(x.Hash, key, value, rest) :> _
                    #else
                    Leaf(x.Hash, key, value, rest) :> _
                    #endif
                | ValueNone ->
                    Empty<'K, 'V>.Instance

        override x.CopyTo(dst : ('K * 'V) array, index : ref<int>) =
            dst.[!index] <- (x.Key, x.Value)
            index := !index + 1
            Linked.copyTo index dst x.Next

        new(h: uint32, k: 'K, v: 'V, n: Linked<'K, 'V>) = { inherit AbstractNode<'K, 'V>(); Hash = h; Key = k; Value = v; Next = n }
     
    #if USE_NO_COLLISION
    and [<Sealed>] NoCollisionLeaf<'K, 'V> =
        inherit AbstractNode<'K, 'V>
        val mutable public Key: 'K
        val mutable public Value: 'V
        val mutable public Hash: uint32
        
        override x.IsEmpty = false
        
        override x.Accept(v: NodeVisitor<_,_,_>) =
            v.VisitNoCollision x

        override x.TryFind(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =   
            if hash = x.Hash && cmp.Equals(key, x.Key) then 
                Some x.Value
            else
                None

        override x.ContainsKey(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =   
            if hash = x.Hash && cmp.Equals(key, x.Key) then 
                true
            else
                false

        override x.Remove(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K) =
            if hash = x.Hash && cmp.Equals(key, x.Key) then
                cnt:= !cnt - 1
                Empty<'K, 'V>.Instance
            else
                x:> _

        override x.TryRemove(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K) =
            if hash = x.Hash && cmp.Equals(key, x.Key) then
                cnt:= !cnt - 1
                ValueSome (struct(x.Value, Empty<'K, 'V>.Instance))
            else
                ValueNone

        override x.AddInPlaceUnsafe(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K, value: 'V) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    x.Key <- key
                    x.Value <- value
                    x:> _
                else
                    cnt:= !cnt + 1
                    Leaf<'K, 'V>(x.Hash, x.Key, x.Value, Linked(key, value, null)) :> _
            else
                cnt:= !cnt + 1
                let n = NoCollisionLeaf<'K, 'V>(hash, key, value)
                Node.Join(hash, n, x.Hash, x)

        override x.Add(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K, value: 'V) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    NoCollisionLeaf<'K, 'V>(x.Hash, key, value) :> _
                else
                    Leaf<'K, 'V>(x.Hash, x.Key, x.Value, Linked.add cmp cnt key value null) :> _
            else
                cnt:= !cnt + 1
                let n = NoCollisionLeaf<'K, 'V>(hash, key, value)
                Node.Join(hash, n, x.Hash, x)
        
        override x.Alter(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K, update: option<'V> -> option<'V>) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    match update (Some x.Value) with
                    | Some value -> 
                        NoCollisionLeaf(x.Hash, x.Key, value) :> _
                    | None -> 
                        cnt:= !cnt - 1
                        Empty.Instance
                else
                    match update None with
                    | None -> x:> _
                    | Some value ->
                        cnt:= !cnt + 1
                        Leaf(x.Hash, x.Key, x.Value, Linked(key, value, null)) :> _
            else
                match update None with
                | None -> x:> _
                | Some value ->
                    cnt:= !cnt + 1
                    let n = NoCollisionLeaf<'K, 'V>(hash, key, value)
                    Node.Join(hash, n, x.Hash, x)
           
        override x.Map(mapping: OptimizedClosures.FSharpFunc<'K, 'V, 'T>) =
            let t = mapping.Invoke(x.Key, x.Value)
            NoCollisionLeaf(x.Hash, x.Key, t) :> _
               
        override x.Choose(cnt: ref<int>, mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>) =
            match mapping.Invoke(x.Key, x.Value) with
            | Some v ->
                cnt:= !cnt + 1
                NoCollisionLeaf(x.Hash, x.Key, v) :> _
            | None ->
                Empty<'K, 'T>.Instance
                
        override x.Filter(cnt: ref<int>, predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            if predicate.Invoke(x.Key, x.Value) then
                cnt:= !cnt + 1
                NoCollisionLeaf(x.Hash, x.Key, x.Value) :> _
            else
                Empty<'K, 'V>.Instance
                
        override x.CopyTo(dst : ('K * 'V) array, index : ref<int>) =
            dst.[!index] <- (x.Key, x.Value)
            index := !index + 1

        new(h: uint32, k: 'K, v: 'V) = { inherit AbstractNode<'K, 'V>(); Hash = h; Key = k; Value = v }
    #endif

    and [<Sealed>] Node<'K, 'V> =
        inherit AbstractNode<'K, 'V>
        val mutable public Prefix: uint32
        val mutable public Mask: Mask
        val mutable public Left: AbstractNode<'K, 'V>
        val mutable public Right: AbstractNode<'K, 'V>

        static member Join (p0 : uint32, t0 : AbstractNode<'K, 'V>, p1 : uint32, t1 : AbstractNode<'K, 'V>) : AbstractNode<'K,'V>=
            let m = getMask p0 p1
            if zeroBit p0 m = 0u then Node(getPrefix p0 m, m, t0, t1) :> AbstractNode<_,_>
            else Node(getPrefix p0 m, m, t1, t0) :> AbstractNode<_,_>

        static member Create(p: uint32, m: Mask, l: AbstractNode<'K, 'V>, r: AbstractNode<'K, 'V>) =
            if r.IsEmpty then l
            elif l.IsEmpty then r
            else Node(p, m, l, r) :> _
            
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

        override x.Remove(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                let l = x.Left.Remove(cmp, cnt, hash, key)
                if l == x.Left then x :> _
                else Node.Create(x.Prefix, x.Mask, l, x.Right)
            elif m = 1u then
                let r = x.Right.Remove(cmp, cnt, hash, key)
                if r == x.Right then x :> _
                else Node.Create(x.Prefix, x.Mask, x.Left, r)
            else
                x:> _

        override x.TryRemove(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                match x.Left.TryRemove(cmp, cnt, hash, key) with
                | ValueSome (struct(value, ll)) ->
                    ValueSome (struct(value, Node.Create(x.Prefix, x.Mask, ll, x.Right)))
                | ValueNone ->
                    ValueNone
            elif m = 1u then
                match x.Right.TryRemove(cmp, cnt, hash, key) with
                | ValueSome (struct(value, rr)) ->
                    ValueSome (struct(value, Node.Create(x.Prefix, x.Mask, x.Left, rr)))
                | ValueNone ->
                    ValueNone
            else
                ValueNone

        override x.AddInPlaceUnsafe(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K, value: 'V) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                x.Left <- x.Left.AddInPlaceUnsafe(cmp, cnt, hash, key, value)
                x:> AbstractNode<_,_>
            elif m = 1u then 
                x.Right <- x.Right.AddInPlaceUnsafe(cmp, cnt, hash, key, value)
                x:> AbstractNode<_,_>
            else
                cnt:= !cnt + 1
                #if USE_NO_COLLISION
                let n = NoCollisionLeaf(hash, key, value)
                #else
                let n = Leaf(hash, key, value, null)
                #endif
                Node.Join(x.Prefix, x, hash, n)

        override x.Add(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K, value: 'V) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                Node(x.Prefix, x.Mask, x.Left.Add(cmp, cnt, hash, key, value), x.Right) :> _
            elif m = 1u then 
                Node(x.Prefix, x.Mask, x.Left, x.Right.Add(cmp, cnt, hash, key, value)) :> _
            else
                cnt:= !cnt + 1
                #if USE_NO_COLLISION
                let n = NoCollisionLeaf(hash, key, value)
                #else
                let n = Leaf(hash, key, value, null)
                #endif
                Node.Join(x.Prefix, x, hash, n)

        override x.Alter(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K, update: option<'V> -> option<'V>) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                let ll = x.Left.Alter(cmp, cnt, hash, key, update)
                if ll == x.Left then x:> _
                else Node(x.Prefix, x.Mask, ll, x.Right) :> _
            elif m = 1u then
                let rr = x.Right.Alter(cmp, cnt, hash, key, update)
                if rr == x.Right then x:> _
                else Node(x.Prefix, x.Mask, x.Left, rr) :> _
            else
                match update None with
                | None -> x:> _
                | Some value ->
                    cnt:= !cnt + 1
                    #if USE_NO_COLLISION
                    let n = NoCollisionLeaf(hash, key, value)
                    #else
                    let n = Leaf(hash, key, value, null)
                    #endif
                    Node.Join(x.Prefix, x, hash, n)
                    
        override x.Map(mapping: OptimizedClosures.FSharpFunc<'K, 'V, 'T>) =
            Node(x.Prefix, x.Mask, x.Left.Map(mapping), x.Right.Map(mapping)) :> _
  
        override x.Choose(cnt: ref<int>, mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>) =
            Node.Create(x.Prefix, x.Mask, x.Left.Choose(cnt, mapping), x.Right.Choose(cnt, mapping))
            
        override x.Filter(cnt: ref<int>, predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            Node.Create(x.Prefix, x.Mask, x.Left.Filter(cnt, predicate), x.Right.Filter(cnt, predicate))
            
        override x.CopyTo(dst : ('K * 'V) array, index : ref<int>) =
            x.Left.CopyTo(dst, index)
            x.Right.CopyTo(dst, index)

        new(p: uint32, m: Mask, l: AbstractNode<'K, 'V>, r: AbstractNode<'K, 'V>) = 
            { inherit AbstractNode<'K, 'V>(); Prefix = p; Mask = m; Left = l; Right = r }



[<Struct>]
type HashMapOkasaki<'K, 'V> internal(cmp: EqualityComparer<'K>, root: AbstractNode<'K, 'V>, cnt: int) =

    static member Empty = HashMapOkasaki<'K, 'V>(EqualityComparer<'K>.Default, Empty.Instance, 0)

    member x.Count = cnt
    member internal x.Root = root
    
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfSeq(elements: seq<'K * 'V>) =  
        let cmp = EqualityComparer<'K>.Default
        let cnt = ref 0
        let mutable r = HashMapOkasakiImplementation.Empty<'K, 'V>.Instance 
        for (k, v) in elements do
            let hash = cmp.GetHashCode k |> uint32
            r <- r.AddInPlaceUnsafe(cmp, cnt, hash, k, v)
        HashMapOkasaki<'K, 'V>(cmp, r, !cnt)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfList(elements: list<'K * 'V>) =  
        let cmp = EqualityComparer<'K>.Default
        let cnt = ref 0
        let mutable r = HashMapOkasakiImplementation.Empty<'K, 'V>.Instance 
        for (k, v) in elements do
            let hash = cmp.GetHashCode k |> uint32
            r <- r.AddInPlaceUnsafe(cmp, cnt, hash, k, v)
        HashMapOkasaki<'K, 'V>(cmp, r, !cnt)
        
    static member OfListUnoptimized(elements: list<'K * 'V>) =  
        let cmp = EqualityComparer<'K>.Default
        let cnt = ref 0
        let mutable r = HashMapOkasakiImplementation.Empty<'K, 'V>.Instance 
        for (k, v) in elements do
            let hash = cmp.GetHashCode k |> uint32
            r <- r.Add(cmp, cnt, hash, k, v)
        HashMapOkasaki<'K, 'V>(cmp, r, !cnt)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfArray(elements: array<'K * 'V>) =  
        let cmp = EqualityComparer<'K>.Default
        let cnt = ref 0
        let mutable r = HashMapOkasakiImplementation.Empty<'K, 'V>.Instance 
        for (k, v) in elements do
            let hash = cmp.GetHashCode k |> uint32
            r <- r.AddInPlaceUnsafe(cmp, cnt, hash, k, v)
        HashMapOkasaki<'K, 'V>(cmp, r, !cnt)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Add(key: 'K, value: 'V) =
        let cnt = ref cnt
        let hash = cmp.GetHashCode key |> uint32
        let newRoot = root.Add(cmp, cnt, hash, key, value)
        HashMapOkasaki(cmp, newRoot, !cnt)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Remove(key: 'K) =
        let cnt = ref cnt
        let hash = cmp.GetHashCode key |> uint32
        let newRoot = root.Remove(cmp, cnt, hash, key)
        HashMapOkasaki(cmp, newRoot, !cnt)
         
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.TryRemove(key: 'K) =
        let cnt = ref cnt
        let hash = cmp.GetHashCode key |> uint32
        match root.TryRemove(cmp, cnt, hash, key) with
        | ValueSome (struct(value, newRoot)) ->
            Some (value, HashMapOkasaki(cmp, newRoot, !cnt))
        | ValueNone ->
            None
         
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.TryFind(key: 'K) =
        let hash = cmp.GetHashCode key |> uint32
        root.TryFind(cmp, hash, key)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ContainsKey(key: 'K) =
        let hash = cmp.GetHashCode key |> uint32
        root.ContainsKey(cmp, hash, key)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Alter(key: 'K, update: option<'V> -> option<'V>) =
        let cnt = ref cnt
        let hash = cmp.GetHashCode key |> uint32
        let newRoot = root.Alter(cmp, cnt, hash, key, update)
        HashMapOkasaki(cmp, newRoot, !cnt)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Map(mapping: 'K -> 'V -> 'T) =
        let mapping = OptimizedClosures.FSharpFunc<'K, 'V, 'T>.Adapt mapping
        let newRoot = root.Map(mapping)
        HashMapOkasaki(cmp, newRoot, cnt)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Choose(mapping: 'K -> 'V -> option<'T>) =
        let cnt = ref 0
        let mapping = OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>.Adapt mapping
        let newRoot = root.Choose(cnt, mapping)
        HashMapOkasaki(cmp, newRoot, !cnt)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Filter(predicate: 'K -> 'V -> bool) =
        let cnt = ref 0
        let predicate = OptimizedClosures.FSharpFunc<'K, 'V, bool>.Adapt predicate
        let newRoot = root.Filter(cnt, predicate)
        HashMapOkasaki(cmp, newRoot, !cnt)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member inline x.ToSeq() =
        x :> seq<_>
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToArray() =
        let arr = Array.zeroCreate cnt
        let index = ref 0
        root.CopyTo(arr, index)
        arr
        
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToList() =
        let arr = Array.zeroCreate cnt
        let index = ref 0
        root.CopyTo(arr, index)
        Array.toList arr

    interface System.Collections.IEnumerable with 
        member x.GetEnumerator() = new HashMapOkasakiEnumerator<_,_>(root) :> _
        
    interface System.Collections.Generic.IEnumerable<'K * 'V> with 
        member x.GetEnumerator() = new HashMapOkasakiEnumerator<_,_>(root) :> _

and internal HashMapOkasakiEnumerator<'K, 'V>(root: AbstractNode<'K, 'V>) =
    let mutable stack = [root]
    let mutable linked: Linked<'K, 'V> = null
    let mutable current = Unchecked.defaultof<'K * 'V>

    member x.MoveNext() =
        if isNull linked then
            match stack with
            | (:? Empty<'K, 'V>) :: rest ->
                stack <- rest 
                x.MoveNext()
            #if USE_NO_COLLISION
            | (:? NoCollisionLeaf<'K, 'V> as l) :: rest ->
                stack <- rest
                current <- l.Key, l.Value
                true
            #endif
            | (:? Leaf<'K, 'V> as l) :: rest -> 
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

    [<GeneralizableValue>]
    let empty<'K, 'V> = HashMapOkasaki<'K, 'V>.Empty

    let inline ofSeq (seq: seq<'K * 'V>) =
        HashMapOkasaki<'K, 'V>.OfSeq seq

    let inline ofList (list: list<'K * 'V>) = 
        HashMapOkasaki<'K, 'V>.OfList list

    let inline ofArray (arr: array<'K * 'V>) = 
        HashMapOkasaki<'K, 'V>.OfArray arr
        
    let inline ofListUnoptimized (list: list<'K * 'V>) = 
        HashMapOkasaki<'K, 'V>.OfListUnoptimized list

    let inline toSeq (map: HashMapOkasaki<'K, 'V>) = 
        map.ToSeq()

    let inline toList (map: HashMapOkasaki<'K, 'V>) = 
        map.ToList()

    let inline toArray (map: HashMapOkasaki<'K, 'V>) = 
        map.ToArray()

    let inline add (key: 'K) (value: 'V) (map: HashMapOkasaki<'K, 'V>) =
        map.Add(key, value)

    let inline remove (key: 'K) (map: HashMapOkasaki<'K, 'V>) =
        map.Remove(key)
        
    let inline tryRemove (key: 'K) (map: HashMapOkasaki<'K, 'V>) =
        map.TryRemove(key)

    let inline tryFind (key: 'K) (map: HashMapOkasaki<'K, 'V>) =
        map.TryFind(key)
        
    let inline containsKey (key: 'K) (map: HashMapOkasaki<'K, 'V>) =
        map.ContainsKey(key)

    let inline alter (key: 'K) (update: option<'V> -> option<'V>) (map: HashMapOkasaki<'K, 'V>) =
        map.Alter(key, update)

    let inline map (mapping: 'K -> 'V -> 'T) (map: HashMapOkasaki<'K, 'V>) =
        map.Map mapping

    let inline choose (mapping: 'K -> 'V -> option<'T>) (map: HashMapOkasaki<'K, 'V>) =
        map.Choose mapping

    let inline filter (predicate: 'K -> 'V -> bool) (map: HashMapOkasaki<'K, 'V>) =
        map.Filter predicate