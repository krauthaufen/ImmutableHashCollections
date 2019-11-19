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
            compare l r
        #else
        compare l r
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
    
        let rec chooseV (cnt: ref<int>) (mapping: OptimizedClosures.FSharpFunc<'K, 'V, ValueOption<'T>>) (n: Linked<'K, 'V>) = 
            if isNull n then
                null
            else 
                match mapping.Invoke(n.Key, n.Value) with
                | ValueSome r -> 
                    cnt:= !cnt + 1
                    Linked(n.Key, r, chooseV cnt mapping n.Next)
                | ValueNone -> 
                    chooseV cnt mapping n.Next
    
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
    
        let rec chooseVTup (cnt: ref<int>) (mapping: OptimizedClosures.FSharpFunc<'K, 'V, ValueOption<struct ('T1 * 'T2)>>) (n: Linked<'K, 'V>) : struct(Linked<'K, 'T1> * Linked<'K, 'T2>) = 
            if isNull n then
                struct (null, null)
            else 
                match mapping.Invoke(n.Key, n.Value) with
                | ValueSome(r1, r2) -> 
                    let struct (n1, n2) = chooseVTup cnt mapping n.Next
                    cnt:= !cnt + 1
                    struct (Linked(n.Key, r1, n1), Linked(n.Key, r2, n2))
                | ValueNone -> 
                    chooseVTup cnt mapping n.Next
    
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

        abstract member Count : int

        abstract member AddInPlaceUnsafe: EqualityComparer<'K> * ref<int> * uint32 * 'K * 'V -> AbstractNode<'K, 'V>
        abstract member Add: EqualityComparer<'K> * ref<int> * uint32 * 'K * 'V -> AbstractNode<'K, 'V>
        abstract member Alter: EqualityComparer<'K> * ref<int> * uint32 * 'K * (option<'V> -> option<'V>) -> AbstractNode<'K, 'V>
        abstract member TryFind: EqualityComparer<'K> * uint32 * 'K -> option<'V>
        abstract member ContainsKey: EqualityComparer<'K> * uint32 * 'K -> bool
        abstract member IsEmpty: bool

        abstract member Map: mapping: OptimizedClosures.FSharpFunc<'K, 'V, 'T> -> AbstractNode<'K, 'T>
        abstract member Choose: cnt: ref<int> * mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>> -> AbstractNode<'K, 'T>
        abstract member ChooseV: cnt: ref<int> * mapping: OptimizedClosures.FSharpFunc<'K, 'V, ValueOption<'T>> -> AbstractNode<'K, 'T>
        abstract member ChooseV2: mapping: OptimizedClosures.FSharpFunc<'K, 'V, struct(ValueOption<'T1> * ValueOption<'T2>)> -> struct (AbstractNode<'K, 'T1> * AbstractNode<'K, 'T2>)
        abstract member Filter: cnt: ref<int> * mapping: OptimizedClosures.FSharpFunc<'K, 'V, bool> -> AbstractNode<'K, 'V>

        abstract member Accept: NodeVisitor<'K, 'V, 'R> -> 'R

        abstract member ToArray: ref<array<struct('K * 'V)>> * ref<int> -> unit

        abstract member CopyTo: dst: ('K * 'V) array * index : ref<int> -> unit

    and [<AbstractClass>] LeafLike<'K, 'V>() =
        inherit AbstractNode<'K, 'V>()
        abstract member LHash : uint32
        abstract member LKey : 'K
        abstract member LValue : 'V
        abstract member LNext : Linked<'K, 'V>

    and [<AbstractClass>] NodeVisitor<'K, 'V, 'R>() =
        abstract member VisitNode: Node<'K, 'V> -> 'R
        abstract member VisitLeaf: Leaf<'K, 'V> -> 'R
        abstract member VisitNoCollision: NoCollisionLeaf<'K, 'V> -> 'R
        abstract member VisitEmpty: Empty<'K, 'V> -> 'R
        
    and [<Sealed>] Empty<'K, 'V> private() =
        inherit AbstractNode<'K, 'V>()
        static let instance = Empty<'K, 'V>() :> AbstractNode<_,_>
        static member Instance = instance

        override x.Count = 0

        override x.ToArray(dst, o) =
            ()

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
            NoCollisionLeaf<'K, 'V>.New(hash, key, value) :> _

        override x.Add(_cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K, value: 'V) =
            cnt:= !cnt + 1
            NoCollisionLeaf<'K, 'V>.New(hash, key, value) :> _

        override x.Alter(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K, update: option<'V> -> option<'V>) =
            match update None with
            | None -> x:> _
            | Some value ->
                cnt:= !cnt + 1
                NoCollisionLeaf<'K, 'V>.New(hash, key, value) :> _

        override x.Map(_mapping: OptimizedClosures.FSharpFunc<'K, 'V, 'T>) =
            Empty<'K, 'T>.Instance
            
        override x.Choose(_cnt: ref<int>, _mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>) =
            Empty<'K, 'T>.Instance
            
        override x.ChooseV(_cnt: ref<int>, _mapping: OptimizedClosures.FSharpFunc<'K, 'V, ValueOption<'T>>) =
            Empty<'K, 'T>.Instance
                 
        override x.ChooseV2(_mapping) =
            struct(Empty.Instance, Empty.Instance)
                          
        override x.Filter(_cnt: ref<int>, _predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            Empty<'K, 'V>.Instance

        override x.CopyTo(_dst : ('K * 'V) array, _index : ref<int>) =
            ()

    and [<Sealed>] Leaf<'K, 'V>() =
        inherit LeafLike<'K, 'V>()

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

        static member inline Create(hash: uint32, key: 'K, value: 'V, next: Linked<'K, 'V>) =
            if isNull next then NoCollisionLeaf.New(hash, key, value) :> AbstractNode<'K, 'V>
            else Leaf.New(hash, key, value, next) :> AbstractNode<'K, 'V>
            
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
                let n = NoCollisionLeaf<'K, 'V>.New(hash, key, value)
                Node.Join(hash, n, x.Hash, x)
                
        override x.Add(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K, value: 'V) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    Leaf<'K, 'V>.New(x.Hash, key, value, x.Next) :> _
                else
                    Leaf<'K, 'V>.New(x.Hash, x.Key, x.Value, Linked.add cmp cnt key value x.Next) :> _
            else
                cnt:= !cnt + 1
                let n = NoCollisionLeaf<'K, 'V>.New(hash, key, value)
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
                        Leaf.New(x.Hash, x.Key, value, x.Next) :> _
                else
                    // in linked?
                    let n = Linked.alter cmp cnt key update x.Next
                    if n == x.Next then x:> _
                    else Leaf.New(x.Hash, x.Key, x.Value, n) :> _
            else
                // other hash => not contained
                match update None with
                | None -> x:> _
                | Some value ->
                    // add
                    cnt:= !cnt + 1
                    let n = NoCollisionLeaf<'K, 'V>.New(hash, key, value)
                    Node.Join(hash, n, x.Hash, x)

        override x.Map(mapping: OptimizedClosures.FSharpFunc<'K, 'V, 'T>) =
            let t = mapping.Invoke(x.Key, x.Value)
            Leaf.New(x.Hash, x.Key, t, Linked.map mapping x.Next) :> AbstractNode<_, _>
            
        override x.Choose(cnt: ref<int>, mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>) =
            match mapping.Invoke(x.Key, x.Value) with
            | Some v ->
                cnt:= !cnt + 1
                Leaf.New(x.Hash, x.Key, v, Linked.choose cnt mapping x.Next) :> _
            | None -> 
                let rest = Linked.choose cnt mapping x.Next
                match Linked.destruct rest with
                | ValueSome (struct (key, value, rest)) ->
                    if isNull rest then NoCollisionLeaf.New(x.Hash, key, value) :> _
                    else Leaf.New(x.Hash, key, value, rest) :> _
                | ValueNone ->
                    Empty<'K, 'T>.Instance

        override x.ChooseV(cnt: ref<int>, mapping: OptimizedClosures.FSharpFunc<'K, 'V, ValueOption<'T>>) =
            match mapping.Invoke(x.Key, x.Value) with
            | ValueSome v ->
                cnt:= !cnt + 1
                Leaf.New(x.Hash, x.Key, v, Linked.chooseV cnt mapping x.Next) :> _
            | ValueNone -> 
                let rest = Linked.chooseV cnt mapping x.Next
                match Linked.destruct rest with
                | ValueSome (struct (key, value, rest)) ->
                    if isNull rest then NoCollisionLeaf.New(x.Hash, key, value) :> _
                    else Leaf.New(x.Hash, key, value, rest) :> _
                | ValueNone ->
                    Empty<'K, 'T>.Instance

        override x.ChooseV2(mapping: OptimizedClosures.FSharpFunc<'K, 'V, struct (ValueOption<'T1> * ValueOption<'T2>)>) =
            let struct (l,r) = mapping.Invoke(x.Key, x.Value)
            let struct (ln, rn) = Linked.chooseV2 mapping x.Next
            let left = 
                match l with
                | ValueSome v -> Leaf.New(x.Hash, x.Key, v, ln) :> AbstractNode<_,_>
                | ValueNone -> 
                    match Linked.destruct ln with
                    | ValueSome (struct (key, value, rest)) ->
                        if isNull rest then NoCollisionLeaf.New(x.Hash, key, value) :> _
                        else Leaf.New(x.Hash, key, value, rest) :> _
                    | ValueNone ->
                        Empty<'K, 'T1>.Instance
            let right = 
                match r with
                | ValueSome v -> Leaf.New(x.Hash, x.Key, v, rn) :> AbstractNode<_,_>
                | ValueNone -> 
                    match Linked.destruct rn with
                    | ValueSome (struct (key, value, rest)) ->
                        if isNull rest then NoCollisionLeaf.New(x.Hash, key, value) :> _
                        else Leaf.New(x.Hash, key, value, rest) :> _
                    | ValueNone ->
                        Empty<'K, 'T2>.Instance
            struct (left, right)

        override x.Filter(cnt: ref<int>, predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            if predicate.Invoke(x.Key, x.Value) then
                cnt:= !cnt + 1
                Leaf.New(x.Hash, x.Key, x.Value, Linked.filter cnt predicate x.Next) :> _
            else
                let rest = Linked.filter cnt predicate x.Next
                match Linked.destruct rest with
                | ValueSome (struct (key, value, rest)) ->
                    if isNull rest then NoCollisionLeaf.New(x.Hash, key, value) :> _
                    else Leaf.New(x.Hash, key, value, rest) :> _
                | ValueNone ->
                    Empty<'K, 'V>.Instance

        override x.CopyTo(dst : ('K * 'V) array, index : ref<int>) =
            dst.[!index] <- (x.Key, x.Value)
            index := !index + 1
            Linked.copyTo index dst x.Next

        static member New(h: uint32, k: 'K, v: 'V, n: Linked<'K, 'V>) : Leaf<'K, 'V> = new Leaf<'K, 'V>(Hash = h, Key = k, Value = v, Next = n)
     
    and [<Sealed>] NoCollisionLeaf<'K, 'V>() =
        inherit LeafLike<'K, 'V>()
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
                    Leaf.New(x.Hash, x.Key, x.Value, Linked(key, value, null)) :> _
            else
                cnt:= !cnt + 1
                let n = NoCollisionLeaf.New(hash, key, value)
                Node.Join(hash, n, x.Hash, x)

        override x.Add(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K, value: 'V) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    NoCollisionLeaf.New(x.Hash, key, value) :> _
                else
                    Leaf.New(x.Hash, x.Key, x.Value, Linked.add cmp cnt key value null) :> _
            else
                cnt:= !cnt + 1
                let n = NoCollisionLeaf.New(hash, key, value)
                Node.Join(hash, n, x.Hash, x)
        
        override x.Alter(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K, update: option<'V> -> option<'V>) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    match update (Some x.Value) with
                    | Some value -> 
                        NoCollisionLeaf.New(x.Hash, x.Key, value) :> _
                    | None -> 
                        cnt:= !cnt - 1
                        Empty.Instance
                else
                    match update None with
                    | None -> x:> _
                    | Some value ->
                        cnt:= !cnt + 1
                        Leaf.New(x.Hash, x.Key, x.Value, Linked(key, value, null)) :> _
            else
                match update None with
                | None -> x:> _
                | Some value ->
                    cnt:= !cnt + 1
                    let n = NoCollisionLeaf.New(hash, key, value)
                    Node.Join(hash, n, x.Hash, x)
           
        override x.Map(mapping: OptimizedClosures.FSharpFunc<'K, 'V, 'T>) =
            let t = mapping.Invoke(x.Key, x.Value)
            NoCollisionLeaf.New(x.Hash, x.Key, t) :> _
               
        override x.Choose(cnt: ref<int>, mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>) =
            match mapping.Invoke(x.Key, x.Value) with
            | Some v ->
                cnt:= !cnt + 1
                NoCollisionLeaf.New(x.Hash, x.Key, v) :> _
            | None ->
                Empty<'K, 'T>.Instance
                
        override x.ChooseV(cnt: ref<int>, mapping: OptimizedClosures.FSharpFunc<'K, 'V, ValueOption<'T>>) =
            match mapping.Invoke(x.Key, x.Value) with
            | ValueSome v ->
                cnt:= !cnt + 1
                NoCollisionLeaf.New(x.Hash, x.Key, v) :> _
            | ValueNone ->
                Empty<'K, 'T>.Instance
 
        override x.ChooseV2(mapping: OptimizedClosures.FSharpFunc<'K, 'V, struct (ValueOption<'T1> * ValueOption<'T2>)>) =
            let struct (l,r) = mapping.Invoke(x.Key, x.Value)         
            let l = match l with | ValueSome v -> NoCollisionLeaf.New(x.Hash, x.Key, v) :> AbstractNode<_,_> | _ -> Empty.Instance
            let r = match r with | ValueSome v -> NoCollisionLeaf.New(x.Hash, x.Key, v) :> AbstractNode<_,_> | _ -> Empty.Instance
            struct (l, r)

        override x.Filter(cnt: ref<int>, predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            if predicate.Invoke(x.Key, x.Value) then
                cnt:= !cnt + 1
                NoCollisionLeaf.New(x.Hash, x.Key, x.Value) :> _
            else
                Empty<'K, 'V>.Instance
                
        override x.CopyTo(dst : ('K * 'V) array, index : ref<int>) =
            dst.[!index] <- (x.Key, x.Value)
            index := !index + 1

        static member New(h: uint32, k: 'K, v: 'V) : NoCollisionLeaf<'K, 'V> = new NoCollisionLeaf<'K, 'V>(Hash = h, Key = k, Value = v)

    and [<Sealed>] Node<'K, 'V>() =
        inherit AbstractNode<'K, 'V>()
        [<DefaultValue>]
        val mutable public Prefix: uint32
        [<DefaultValue>]
        val mutable public Mask: Mask
        [<DefaultValue>]
        val mutable public Left: AbstractNode<'K, 'V>
        [<DefaultValue>]
        val mutable public Right: AbstractNode<'K, 'V>
        
        override x.Count =
            x.Left.Count + x.Right.Count

        static member Join (p0 : uint32, t0 : AbstractNode<'K, 'V>, p1 : uint32, t1 : AbstractNode<'K, 'V>) : AbstractNode<'K,'V>=
            let m = getMask p0 p1
            if zeroBit p0 m = 0u then Node.New(getPrefix p0 m, m, t0, t1) :> AbstractNode<_,_>
            else Node.New(getPrefix p0 m, m, t1, t0) :> AbstractNode<_,_>

        static member Create(p: uint32, m: Mask, l: AbstractNode<'K, 'V>, r: AbstractNode<'K, 'V>) =
            if r.IsEmpty then l
            elif l.IsEmpty then r
            else Node.New(p, m, l, r) :> _

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
                let n = NoCollisionLeaf.New(hash, key, value)
                Node.Join(x.Prefix, x, hash, n)

        override x.Add(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K, value: 'V) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                Node.New(x.Prefix, x.Mask, x.Left.Add(cmp, cnt, hash, key, value), x.Right) :> _
            elif m = 1u then 
                Node.New(x.Prefix, x.Mask, x.Left, x.Right.Add(cmp, cnt, hash, key, value)) :> _
            else
                cnt:= !cnt + 1
                let n = NoCollisionLeaf.New(hash, key, value)
                Node.Join(x.Prefix, x, hash, n)

        override x.Alter(cmp: EqualityComparer<'K>, cnt: ref<int>, hash: uint32, key: 'K, update: option<'V> -> option<'V>) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                let ll = x.Left.Alter(cmp, cnt, hash, key, update)
                if ll == x.Left then x:> _
                else Node.New(x.Prefix, x.Mask, ll, x.Right) :> _
            elif m = 1u then
                let rr = x.Right.Alter(cmp, cnt, hash, key, update)
                if rr == x.Right then x:> _
                else Node.New(x.Prefix, x.Mask, x.Left, rr) :> _
            else
                match update None with
                | None -> x:> _
                | Some value ->
                    cnt:= !cnt + 1
                    let n = NoCollisionLeaf.New(hash, key, value)
                    Node.Join(x.Prefix, x, hash, n)
                    
        override x.Map(mapping: OptimizedClosures.FSharpFunc<'K, 'V, 'T>) =
            Node.New(x.Prefix, x.Mask, x.Left.Map(mapping), x.Right.Map(mapping)) :> _
  
        override x.Choose(cnt: ref<int>, mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>) =
            Node.Create(x.Prefix, x.Mask, x.Left.Choose(cnt, mapping), x.Right.Choose(cnt, mapping))
            
        override x.ChooseV(cnt: ref<int>, mapping: OptimizedClosures.FSharpFunc<'K, 'V, ValueOption<'T>>) =
            Node.Create(x.Prefix, x.Mask, x.Left.ChooseV(cnt, mapping), x.Right.ChooseV(cnt, mapping))
      
        override x.ChooseV2(mapping: OptimizedClosures.FSharpFunc<'K, 'V, struct(ValueOption<'T1> * ValueOption<'T2>)>) =
            let struct (la, lb) = x.Left.ChooseV2(mapping)
            let struct (ra, rb) = x.Right.ChooseV2(mapping)

            struct (
                Node.Create(x.Prefix, x.Mask, la, ra),
                Node.Create(x.Prefix, x.Mask, lb, rb)
            )
      
        override x.Filter(cnt: ref<int>, predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            Node.Create(x.Prefix, x.Mask, x.Left.Filter(cnt, predicate), x.Right.Filter(cnt, predicate))
            
        override x.CopyTo(dst : ('K * 'V) array, index : ref<int>) =
            x.Left.CopyTo(dst, index)
            x.Right.CopyTo(dst, index)

        static member New(p: uint32, m: Mask, l: AbstractNode<'K, 'V>, r: AbstractNode<'K, 'V>) : Node<'K, 'V> = 
            new Node<'K, 'V>(Prefix = p, Mask = m, Left = l, Right = r)

    and [<AbstractClass>] NodeVisitor2<'K, 'V1, 'V2, 'R>() =
        abstract member VisitNN     : Node<'K, 'V1> * Node<'K, 'V2> -> 'R

        abstract member VisitNL     : Node<'K, 'V1> * LeafLike<'K, 'V2> -> 'R
        abstract member VisitLN     : LeafLike<'K, 'V1> * Node<'K, 'V2> -> 'R
        abstract member VisitLL     : LeafLike<'K, 'V1> * LeafLike<'K, 'V2> -> 'R

        abstract member VisitAE     : AbstractNode<'K, 'V1> * Empty<'K, 'V2> -> 'R
        abstract member VisitEA     : Empty<'K, 'V1> * AbstractNode<'K, 'V2> -> 'R
        abstract member VisitEE     : Empty<'K, 'V1> * Empty<'K, 'V2> -> 'R

    type Visit2Visitor<'K, 'V1, 'V2, 'R>(real : NodeVisitor2<'K, 'V1, 'V2, 'R>, node : AbstractNode<'K, 'V2>) =
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
        let visit (v : NodeVisitor2<'K, 'V1, 'V2, 'R>) (l : AbstractNode<'K, 'V1>) (r : AbstractNode<'K, 'V2>) =
            l.Accept (Visit2Visitor(v, r))



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
    static member ComputeDelta(l : HashMapOkasaki<'K, 'V>, r : HashMapOkasaki<'K, 'V>, add : 'K -> 'V -> 'OP, update : 'K -> 'V -> 'V -> ValueOption<'OP>, remove : 'K -> 'V -> 'OP) =   
        let cnt = ref 0
        let add = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(add)
        let remove = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(remove)
        let update = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(update)

        let add = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(fun k v -> cnt := !cnt + 1; add.Invoke(k,v))
        let remove = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(fun k v -> cnt := !cnt + 1; remove.Invoke(k,v))
        let update = 
            OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(fun k l r -> 
                match update.Invoke(k,l,r) with 
                | ValueSome v -> cnt := !cnt + 1; ValueSome v 
                | ValueNone -> ValueNone
            )
        
        let len = ref 0
        let arr = ref (Array.zeroCreate 4)
        let foo = ref 0
        let cmp = EqualityComparer<'K>.Default

        let result = 
            let cnt = ()
            (l.Root, r.Root) ||> Visit2.visit {
                new NodeVisitor2<'K, 'V, 'V, AbstractNode<'K, 'OP>>() with

                    member x.VisitEE(_, _) = Empty<'K, 'OP>.Instance
                    member x.VisitEA(_, r) = r.Map(add)
                    member x.VisitAE(l, _) = l.Map(remove)

                    member x.VisitLL(l, r) = 
                        if l == r then
                            Empty<'K, 'OP>.Instance
                        else
                            len := 0
                            if l.LHash = r.LHash then
                                let mutable r = r :> AbstractNode<_,_>
                                let mutable res = Empty<'K, 'OP>.Instance
                                let hash = l.LHash
                        
                                l.ToArray(arr, len)
                                for i in 0 .. !len - 1 do
                                    let struct (k, lv) = arr.Value.[i]
                                    match r.TryRemove(cmp, foo, hash, k) with
                                    | ValueSome (rv, rest) ->
                                        r <- rest
                                        match update.Invoke(k, lv, rv) with
                                        | ValueSome op ->
                                            res <- res.AddInPlaceUnsafe(cmp, foo, hash, k, op)
                                        | ValueNone ->
                                            ()
                                    | ValueNone ->
                                        res <- res.AddInPlaceUnsafe(cmp, foo, hash, k, remove.Invoke(k, lv))

                                len := 0
                                r.ToArray(arr, len)
                                for i in 0 .. !len - 1 do
                                    let struct (k, rv) = arr.Value.[i]
                                    res <- res.AddInPlaceUnsafe(cmp, foo, hash, k, add.Invoke(k, rv))
                        
                                res
                            else
                                let mutable res = l.Map(remove)
                                r.ToArray(arr, len)
                                for i in 0 .. !len - 1 do
                                    let struct (k, rv) = arr.Value.[i]
                                    res <- res.AddInPlaceUnsafe(cmp, foo, r.LHash, k, add.Invoke(k, rv))
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

        HashMapOkasaki(cmp, result, !cnt)
 
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member UnionWith(l : HashMapOkasaki<'K, 'V>, r : HashMapOkasaki<'K, 'V>, resolve : 'K -> 'V -> 'V -> 'V) =   
        let cnt = ref (l.Count + r.Count)
        let resolve = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(resolve)
        let update = 
            OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(fun k l r -> 
                cnt := !cnt - 1
                resolve.Invoke(k, l, r)
            )
    
        let len = ref 0
        let arr = ref (Array.zeroCreate 4)
        let foo = ref 0
        let cmp = EqualityComparer<'K>.Default

        let result = 
            let cnt = ()
            (l.Root, r.Root) ||> Visit2.visit {
                new NodeVisitor2<'K, 'V, 'V, AbstractNode<'K, 'V>>() with

                    member x.VisitEE(_, _) = Empty<'K, 'V>.Instance
                    member x.VisitEA(_, r) = r
                    member x.VisitAE(l, _) = l

                    member x.VisitLL(l, r) = 
                        len := 0
                        if l.LHash = r.LHash then
                            let mutable r = r :> AbstractNode<_,_>
                            let mutable res = Empty<'K, 'V>.Instance
                            let hash = l.LHash
                    
                            l.ToArray(arr, len)
                            for i in 0 .. !len - 1 do
                                let struct (k, lv) = arr.Value.[i]
                                match r.TryRemove(cmp, foo, hash, k) with
                                | ValueSome (rv, rest) ->
                                    r <- rest
                                    let op = update.Invoke(k, lv, rv)
                                    res <- res.AddInPlaceUnsafe(cmp, foo, hash, k, op)
                                | ValueNone ->
                                    res <- res.AddInPlaceUnsafe(cmp, foo, hash, k, lv)

                            len := 0
                            r.ToArray(arr, len)
                            for i in 0 .. !len - 1 do
                                let struct (k, rv) = arr.Value.[i]
                                res <- res.AddInPlaceUnsafe(cmp, foo, hash, k, rv)
                    
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

        HashMapOkasaki(cmp, result, !cnt)
  
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Union(l : HashMapOkasaki<'K, 'V>, r : HashMapOkasaki<'K, 'V>) =   
        let cnt = ref 0

        let len = ref 0
        let arr = ref (Array.zeroCreate 4)
        let foo = ref 0
        let cmp = EqualityComparer<'K>.Default

        let result = 
            (l.Root, r.Root) ||> Visit2.visit {
                new NodeVisitor2<'K, 'V, 'V, AbstractNode<'K, 'V>>() with

                    member x.VisitEE(_, _) = Empty<'K, 'V>.Instance
                    member x.VisitEA(_, r) = 
                        cnt := !cnt + r.Count
                        r
                    member x.VisitAE(l, _) = 
                        cnt := !cnt + l.Count
                        l

                    member x.VisitLL(l, r) = 
                        if l == r then
                            cnt := !cnt + l.Count
                            r :> AbstractNode<_,_>
                        else
                            len := 0
                            if l.LHash = r.LHash then
                                let mutable r = r :> AbstractNode<_,_>
                                let mutable res = Empty<'K, 'V>.Instance
                                let hash = l.LHash
                
                                l.ToArray(arr, len)
                                for i in 0 .. !len - 1 do
                                    let struct (k, lv) = arr.Value.[i]
                                    match r.TryRemove(cmp, foo, hash, k) with
                                    | ValueSome (rv, rest) ->
                                        r <- rest
                                        res <- res.AddInPlaceUnsafe(cmp, foo, hash, k, rv)
                                        cnt := !cnt + 1
                                    | ValueNone ->
                                        res <- res.AddInPlaceUnsafe(cmp, foo, hash, k, lv)
                                        cnt := !cnt + 1

                                len := 0
                                r.ToArray(arr, len)
                                for i in 0 .. !len - 1 do
                                    let struct (k, rv) = arr.Value.[i]
                                    res <- res.AddInPlaceUnsafe(cmp, foo, hash, k, rv)
                                    cnt := !cnt + 1
                
                                res
                            else
                                cnt := !cnt + l.Count + r.Count
                                Node.Join(l.LHash, l, r.LHash, r)
                     

                    member x.VisitLN(l, r) =
                        let b = matchPrefixAndGetBit l.LHash r.Prefix r.Mask
                        if b = 0u then
                            cnt := !cnt + r.Right.Count
                            Node.Create(r.Prefix, r.Mask, Visit2.visit x l r.Left, r.Right)
                        elif b = 1u then
                            cnt := !cnt + r.Left.Count
                            Node.Create(r.Prefix, r.Mask, r.Left, Visit2.visit x l r.Right)
                        else
                            cnt := !cnt + l.Count + r.Count
                            Node.Join(l.LHash, l, r.Prefix, r)

                    member x.VisitNL(l, r) =
                        let b = matchPrefixAndGetBit r.LHash l.Prefix l.Mask
                        if b = 0u then
                            cnt := !cnt + l.Right.Count
                            Node.Create(l.Prefix, l.Mask, Visit2.visit x l.Left r, l.Right)
                        elif b = 1u then
                            cnt := !cnt + l.Left.Count
                            Node.Create(l.Prefix, l.Mask, l.Left, Visit2.visit x l.Right r)
                        else
                            cnt := !cnt + l.Count + r.Count
                            Node.Join(l.Prefix, l, r.LHash, r)

                    member x.VisitNN(l, r) = 
                        if l == r then 
                            cnt := !cnt + r.Count
                            r :> AbstractNode<_,_>
                        else
                            let cc = compareMasks l.Mask r.Mask
                            if cc = 0 then
                                let l' = (l.Left, r.Left) ||> Visit2.visit x
                                let r' = (l.Right, r.Right) ||> Visit2.visit x
                                Node.Create(l.Prefix, l.Mask, l', r')
                            elif cc > 0 then
                                let lr = matchPrefixAndGetBit l.Prefix r.Prefix r.Mask
                                if lr = 0u then
                                    cnt := !cnt + r.Right.Count
                                    Node.Create(r.Prefix, r.Mask, Visit2.visit x l r.Left, r.Right)
                                elif lr = 1u then
                                    cnt := !cnt + r.Left.Count
                                    Node.Create(r.Prefix, r.Mask, r.Left, Visit2.visit x l r.Right)
                                else
                                    cnt := !cnt + l.Count + r.Count
                                    Node.Join(l.Prefix, l, r.Prefix, r)
                            else
                                let rl = matchPrefixAndGetBit r.Prefix l.Prefix l.Mask
                    
                                if rl = 0u then
                                    cnt := !cnt + l.Right.Count
                                    Node.Create(l.Prefix, l.Mask, Visit2.visit x l.Left r, l.Right)
                                elif rl = 1u then
                                    cnt := !cnt + l.Left.Count
                                    Node.Create(l.Prefix, l.Mask, l.Left, Visit2.visit x l.Right r)
                                else
                                    cnt := !cnt + l.Count + r.Count
                                    Node.Join(l.Prefix, l, r.Prefix, r)
                            
            }

        HashMapOkasaki(cmp, result, !cnt)
  

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ApplyDelta(state : HashMapOkasaki<'K, 'V>, delta : HashMapOkasaki<'K, 'D>, apply : 'K -> voption<'V> -> 'D -> struct(voption<'V> * voption<'D>)) =   
        let apply = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(apply)

        let onlyDelta = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(fun k d -> apply.Invoke(k, ValueNone, d))
    
        let len = ref 0
        let arr1 = ref (Array.zeroCreate 4)
        let arr2 = ref (Array.zeroCreate 4)
        let foo = ref 0
        let cmp = EqualityComparer<'K>.Default

        let struct(result, delta) = 
            let cnt = ()
            (state.Root, delta.Root) ||> Visit2.visit {
                new NodeVisitor2<'K, 'V, 'D, struct(AbstractNode<'K, 'V> * AbstractNode<'K, 'D>)>() with

                    member x.VisitEE(_, _) = 
                        struct (Empty.Instance, Empty.Instance)

                    member x.VisitEA(_, r) =    
                        r.ChooseV2 onlyDelta

                    member x.VisitAE(l, _) = 
                        struct(l, Empty.Instance)

                    member x.VisitLL(state, delta) = 
                        len := 0
                        if state.LHash = delta.LHash then
                            let mutable delta = delta :> AbstractNode<_,_>
                            let mutable resState = Empty<'K, 'V>.Instance
                            let mutable resDelta = Empty<'K, 'D>.Instance
                            let hash = state.LHash
                    
                            state.ToArray(arr1, len)
                            for i in 0 .. !len - 1 do
                                let struct (k, value) = arr1.Value.[i]
                                match delta.TryRemove(cmp, foo, hash, k) with
                                | ValueSome (dd, rest) ->
                                    delta <- rest
                                    let struct (s, d) = apply.Invoke(k, ValueSome value, dd)

                                    match s with
                                    | ValueSome v -> resState <- resState.AddInPlaceUnsafe(cmp, foo, hash, k, v)
                                    | ValueNone -> ()

                                    match d with
                                    | ValueSome v -> resDelta <- resDelta.AddInPlaceUnsafe(cmp, foo, hash, k, v)
                                    | ValueNone -> ()

                                | ValueNone ->
                                    resState <- resState.AddInPlaceUnsafe(cmp, foo, hash, k, value)

                            len := 0
                            delta.ToArray(arr2, len)
                            for i in 0 .. !len - 1 do
                                let struct (k, rv) = arr2.Value.[i]
                                let struct (s, d) = onlyDelta.Invoke(k, rv)
                                match s with
                                | ValueSome v -> resState <- resState.AddInPlaceUnsafe(cmp, foo, hash, k, v)
                                | ValueNone -> ()
                                match d with
                                | ValueSome v -> resDelta <- resDelta.AddInPlaceUnsafe(cmp, foo, hash, k, v)
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

        HashMapOkasaki(cmp, result, result.Count), HashMapOkasaki(cmp, delta, delta.Count)
 

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
            | (:? NoCollisionLeaf<'K, 'V> as l) :: rest ->
                stack <- rest
                current <- l.Key, l.Value
                true
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

    let inline unionWith (resolve : 'K -> 'V -> 'V -> 'V) (l : HashMapOkasaki<'K, 'V>) (r : HashMapOkasaki<'K, 'V>) =
        let resolve = OptimizedClosures.FSharpFunc<'K, 'V, 'V, 'V>.Adapt resolve
        HashMapOkasaki<'K, 'V>.UnionWith(l, r, fun k l r -> resolve.Invoke(k, l, r))
        
    let inline union (l : HashMapOkasaki<'K, 'V>) (r : HashMapOkasaki<'K, 'V>) =
        HashMapOkasaki<'K, 'V>.Union(l, r)

    let a (l : HashMapOkasaki<'K, 'A>) (r : HashMapOkasaki<'K, 'B>) (mapping : 'K -> option<'A> -> option<'B> -> option<'C> * option<'D>) : HashMapOkasaki<'K, 'C> * HashMapOkasaki<'K, 'D> =
        failwith ""

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