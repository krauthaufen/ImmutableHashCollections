namespace ImmutableHashCollections

open System.Collections
open System.Collections.Generic
open System.Runtime.CompilerServices
#if NETCOREAPP3_0 && USE_INTRINSICS
open System.Runtime.Intrinsics.X86
#endif

[<AutoOpen>]
module internal HashSetOkasakiImplementation = 

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
    type Linked<'K> =
        val mutable public Next: Linked<'K>
        val mutable public Key: 'K

        new(k) = { Key = k; Next = null }
        new(k, n) = { Key = k; Next = n }

    module Linked =
    
        let rec addInPlaceUnsafe (cmp: EqualityComparer<'K>) (key: 'K) (n: Linked<'K>) =
            if isNull n then
                Linked<_>(key)
            elif cmp.Equals(n.Key, key) then
                n.Key <- key
                n
            else
                n.Next <- addInPlaceUnsafe cmp key n.Next
                n

        let rec add (cmp: EqualityComparer<'K>) (key: 'K) (n: Linked<'K>) =
            if isNull n then
                Linked<_>(key)
            elif cmp.Equals(n.Key, key) then
                Linked<_>(key, n.Next)
            else
                Linked<_>(n.Key, add cmp key n.Next)
               
        let rec alter (cmp: EqualityComparer<'K>) (key: 'K) (update: bool -> bool) (n: Linked<'K>) =
            if isNull n then
                match update false with
                | true -> 
                    Linked<_>(key)
                | false ->
                    null
            elif cmp.Equals(n.Key, key) then
                match update true with
                | true -> n
                | false -> n.Next
            else
                let next = alter cmp key update n.Next
                if next == n.Next then n
                else Linked<_>(n.Key, next)

        let rec contains (cmp: EqualityComparer<'K>) (key: 'K) (n: Linked<'K>) =
            if isNull n then false
            elif cmp.Equals(n.Key, key) then true
            else contains cmp key n.Next

        let destruct (n: Linked<'K>) =
            if isNull n then ValueNone
            else ValueSome(struct (n.Key, n.Next))
            
        let rec remove (cmp: EqualityComparer<'K>) (key: 'K) (n: Linked<'K>) =
            if isNull n then
                null
            elif cmp.Equals(n.Key, key) then 
                n.Next
            else
                let rest = remove cmp key n.Next
                if rest == n.Next then n
                else Linked<_>(n.Key, rest)

        let rec tryRemove (cmp: EqualityComparer<'K>) (key: 'K) (n: Linked<'K>) =
            if isNull n then
                ValueNone
            elif cmp.Equals(n.Key, key) then 
                ValueSome (n.Next)
            else
                match tryRemove cmp key n.Next with
                | ValueSome rest ->
                    ValueSome(Linked<_>(n.Key, rest))
                | ValueNone ->
                    ValueNone
    
        let rec filter (predicate: 'K -> bool) (n: Linked<'K>) =
            if isNull n then
                null
            elif predicate n.Key then
                Linked<_>(n.Key, filter predicate n.Next)
            else
                filter predicate n.Next
    
        let rec exists (predicate: 'K -> bool) (n: Linked<'K>) =
            if isNull n then 
                false
            elif predicate n.Key then
                true
            else
                exists predicate n.Next
                
        let rec forall (predicate: 'K -> bool) (n: Linked<'K>) =
            if isNull n then 
                true
            elif not (predicate n.Key) then
                false
            else
                forall predicate n.Next

        let rec copyTo (index: ref<int>) (dst : 'K array) (n: Linked<'K>) =
            if not (isNull n) then
                dst.[!index] <- n.Key
                index := !index + 1
                copyTo index dst n.Next
    

    [<AbstractClass>]
    type AbstractNode<'K>() =
        abstract member Remove: EqualityComparer<'K> * uint32 * 'K -> AbstractNode<'K>
        abstract member TryRemove: EqualityComparer<'K> * uint32 * 'K -> ValueOption<AbstractNode<'K>>

        abstract member Count : int

        abstract member AddInPlaceUnsafe: EqualityComparer<'K> * uint32 * 'K -> AbstractNode<'K>
        abstract member Add: EqualityComparer<'K> * uint32 * 'K -> AbstractNode<'K>
        abstract member Alter: EqualityComparer<'K> * uint32 * 'K * (bool -> bool) -> AbstractNode<'K>
        abstract member Contains: EqualityComparer<'K> * uint32 * 'K -> bool
        abstract member IsEmpty: bool

        abstract member Filter: mapping: ('K -> bool) -> AbstractNode<'K>
        abstract member Iter: action: ('K -> unit) -> unit
        abstract member Fold: acc: OptimizedClosures.FSharpFunc<'S, 'K, 'S> * seed : 'S -> 'S
        abstract member Exists: predicate: ('K -> bool) -> bool
        abstract member Forall: predicate: ('K -> bool) -> bool

        abstract member Accept: NodeVisitor<'K, 'R> -> 'R

        abstract member ToArray: ref<array<'K>> * ref<int> -> unit

        abstract member CopyTo: dst: 'K array * index : ref<int> -> unit

    and [<AbstractClass>] Leaf<'K>() =
        inherit AbstractNode<'K>()
        abstract member LHash : uint32
        abstract member LKey : 'K
        abstract member LNext : Linked<'K>

    and [<AbstractClass>] NodeVisitor<'K, 'R>() =
        abstract member VisitNode: Node<'K> -> 'R
        abstract member VisitLeaf: CollisionLeaf<'K> -> 'R
        abstract member VisitNoCollision: NoCollisionLeaf<'K> -> 'R
        abstract member VisitEmpty: Empty<'K> -> 'R
        
    and [<Sealed>] Empty<'K> private() =
        inherit AbstractNode<'K>()
        static let instance = Empty<'K>() :> AbstractNode<_>
        static member Instance = instance

        override x.Count = 0

        override x.ToArray(dst, o) =
            ()

        override x.Accept(v: NodeVisitor<_,_>) =
            v.VisitEmpty x

        override x.IsEmpty = true

        override x.Contains(_cmp: EqualityComparer<'K>, _hash: uint32, _key: 'K) =
            false

        override x.Remove(_cmp: EqualityComparer<'K>, _hash: uint32, _key: 'K) =
            x:> _
            
        override x.TryRemove(_cmp: EqualityComparer<'K>, _hash: uint32, _key: 'K) =
            ValueNone

        override x.AddInPlaceUnsafe(_cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            NoCollisionLeaf<'K>.New(hash, key)

        override x.Add(_cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            NoCollisionLeaf<'K>.New(hash, key)

        override x.Alter(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, update: bool -> bool) =
            match update false with
            | false -> x:> _
            | true -> NoCollisionLeaf<'K>.New(hash, key)
                
        override x.Filter(_predicate: 'K -> bool) =
            Empty<'K>.Instance

        override x.Iter(_action: 'K -> unit) =
            ()
            
        override x.Fold(_acc: OptimizedClosures.FSharpFunc<'S, 'K, 'S>, seed : 'S) =
            seed

        override x.Exists(_predicate: 'K -> bool) =
            false

        override x.Forall(_predicate: 'K -> bool) =
            true

        override x.CopyTo(_dst : 'K array, _index : ref<int>) =
            ()

    and [<Sealed>] CollisionLeaf<'K> private() =
        inherit Leaf<'K>()

        [<DefaultValue>]
        val mutable public Next: Linked<'K>
        [<DefaultValue>]
        val mutable public Key: 'K
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
        override x.LNext = x.Next

        override x.ToArray(dst, o) =
            if !o >= dst.Value.Length then System.Array.Resize(&dst.contents, !o * 2)
            dst.Value.[!o] <- x.Key
            o := !o + 1
            
            let mutable n = x.Next
            while not (isNull n) do
                if !o >= dst.Value.Length then System.Array.Resize(&dst.contents, !o * 2)
                dst.Value.[!o] <- n.Key
                o := !o + 1
                n <- n.Next

        member x.GetEntries() =
            let mutable arr = Array.zeroCreate 8
            arr.[0] <- x.Key
            let mutable cnt = 1

            let mutable n = x.Next
            while not (isNull n) do
                if cnt >= arr.Length then System.Array.Resize(&arr, cnt * 2)
                arr.[cnt] <- n.Key
                cnt <- cnt + 1
                n <- n.Next
            if cnt < arr.Length then System.Array.Resize(&arr, cnt)
            arr

        override x.Accept(v: NodeVisitor<_,_>) =
            v.VisitLeaf x

        override x.IsEmpty = false

        override x.Contains(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =   
            if hash = x.Hash then
                if cmp.Equals(key, x.Key) then 
                    true
                else
                    Linked.contains cmp key x.Next
            else
                false

        override x.Remove(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            if hash = x.Hash then
                if cmp.Equals(key, x.Key) then
                    match Linked.destruct x.Next with
                    | ValueSome (struct (k, rest)) ->
                        CollisionLeaf<_>.New(hash, k, rest)
                    | ValueNone ->
                        Empty<'K>.Instance
                else
                    let next = Linked.remove cmp key x.Next
                    if next == x.Next then x :> _
                    else CollisionLeaf<_>.New(x.Hash, x.Key, Linked.remove cmp key x.Next)
            else
                x:> _

        override x.TryRemove(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            if hash = x.Hash then
                if cmp.Equals(key, x.Key) then
                    match Linked.destruct x.Next with
                    | ValueSome (struct(k, rest)) ->
                        ValueSome(CollisionLeaf<_>.New(hash, k, rest))
                    | ValueNone ->
                        ValueSome Empty<_>.Instance
                else
                    match Linked.tryRemove cmp key x.Next with
                    | ValueSome rest ->
                        ValueSome(CollisionLeaf<_>.New(x.Hash, x.Key, rest))
                    | ValueNone ->
                        ValueNone
            else
                ValueNone

        override x.AddInPlaceUnsafe(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    x.Key <- key
                    x:> _
                else
                    x.Next <- Linked.addInPlaceUnsafe cmp key x.Next
                    x:> _
            else
                let n = NoCollisionLeaf<'K>.New(hash, key)
                Node<_>.Join(hash, n, x.Hash, x)
                
        override x.Add(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    x :> _
                else
                    CollisionLeaf<_>.New(x.Hash, x.Key, Linked.add cmp key x.Next)
            else
                let n = NoCollisionLeaf<'K>.New(hash, key)
                Node<_>.Join(hash, n, x.Hash, x)

        override x.Alter(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, update: bool -> bool) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    match update true with
                    | false ->
                        // remove
                        match Linked.destruct x.Next with
                        | ValueSome (struct (k, rest)) ->
                            CollisionLeaf<_>.New(x.Hash, k, rest)
                        | ValueNone ->
                            Empty<'K>.Instance
                    | true ->
                        // update
                        x :> _
                else
                    // in linked?
                    let n = Linked.alter cmp key update x.Next
                    if n == x.Next then x:> _
                    else CollisionLeaf<_>.New(x.Hash, x.Key, n)
            else
                // other hash => not contained
                match update false with
                | false -> x:> _
                | true ->
                    // add
                    let n = NoCollisionLeaf<'K>.New(hash, key)
                    Node<_>.Join(hash, n, x.Hash, x)

        override x.Filter(predicate: 'K -> bool) =
            if predicate x.Key then
                CollisionLeaf<_>.New(x.Hash, x.Key, Linked.filter predicate x.Next)
            else
                let rest = Linked.filter predicate x.Next
                match Linked.destruct rest with
                | ValueSome (struct (key, rest)) ->
                    if isNull rest then NoCollisionLeaf<_>.New(x.Hash, key)
                    else CollisionLeaf<_>.New(x.Hash, key, rest)
                | ValueNone ->
                    Empty<'K>.Instance

        override x.Iter(action: 'K -> unit) =
            action x.Key
            let mutable n = x.Next
            while not (isNull n) do
                action n.Key
                n <- n.Next
                
        override x.Fold(acc: OptimizedClosures.FSharpFunc<'S, 'K, 'S>, seed : 'S) =
            let mutable res = acc.Invoke(seed, x.Key)
            let mutable n = x.Next
            while not (isNull n) do
                res <- acc.Invoke(res, n.Key)
                n <- n.Next
            res

        override x.Exists(predicate: 'K -> bool) =
            if predicate x.Key then true
            else Linked.exists predicate x.Next
                
        override x.Forall(predicate: 'K -> bool) =
            if predicate x.Key then Linked.forall predicate x.Next
            else false

        override x.CopyTo(dst : 'K array, index : ref<int>) =
            dst.[!index] <- x.Key
            index := !index + 1
            Linked.copyTo index dst x.Next

        static member New(h: uint32, k: 'K, n: Linked<'K>) : AbstractNode<'K> = 
            if isNull n then NoCollisionLeaf<'K>.New(h, k)
            else new CollisionLeaf<'K>(Hash = h, Key = k, Next = n) :> AbstractNode<'K>
     
    and [<Sealed>] NoCollisionLeaf<'K> private() =
        inherit Leaf<'K>()
        [<DefaultValue>]
        val mutable public Key: 'K
        [<DefaultValue>]
        val mutable public Hash: uint32

        override x.Count = 1
        override x.LHash = x.Hash
        override x.LKey = x.Key
        override x.LNext = null

        override x.ToArray(dst, o) =
            if !o >= dst.Value.Length then System.Array.Resize(&dst.contents, !o * 2)
            dst.Value.[!o] <- x.Key
            o := !o + 1
        
        override x.IsEmpty = false
        
        override x.Accept(v: NodeVisitor<_,_>) =
            v.VisitNoCollision x

        override x.Contains(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =   
            if hash = x.Hash && cmp.Equals(key, x.Key) then 
                true
            else
                false

        override x.Remove(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            if hash = x.Hash && cmp.Equals(key, x.Key) then
                Empty<'K>.Instance
            else
                x:> _

        override x.TryRemove(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            if hash = x.Hash && cmp.Equals(key, x.Key) then
                ValueSome Empty<'K>.Instance
            else
                ValueNone

        override x.AddInPlaceUnsafe(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    x.Key <- key
                    x:> _
                else
                    CollisionLeaf<_>.New(x.Hash, x.Key, Linked<_>(key, null))
            else
                let n = NoCollisionLeaf<_>.New(hash, key)
                Node<_>.Join(hash, n, x.Hash, x)

        override x.Add(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    NoCollisionLeaf<_>.New(x.Hash, key)
                else
                    CollisionLeaf<_>.New(x.Hash, x.Key, Linked.add cmp key null)
            else
                let n = NoCollisionLeaf<_>.New(hash, key)
                Node<_>.Join(hash, n, x.Hash, x)
        
        override x.Alter(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, update: bool -> bool) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    match update true with
                    | true -> 
                        x :> _
                    | false -> 
                        Empty<_>.Instance
                else
                    match update false with
                    | false -> x:> _
                    | true ->
                        CollisionLeaf<_>.New(x.Hash, x.Key, Linked<_>(key, null))
            else
                match update false with
                | false -> x:> _
                | true ->
                    let n = NoCollisionLeaf<_>.New(hash, key)
                    Node<_>.Join(hash, n, x.Hash, x)
           
        override x.Filter(predicate: 'K -> bool) =
            if predicate x.Key then
                NoCollisionLeaf<_>.New(x.Hash, x.Key)
            else
                Empty<'K>.Instance
 
        override x.Iter(action: 'K -> unit) =
            action x.Key
            
        override x.Fold(acc: OptimizedClosures.FSharpFunc<'S, 'K, 'S>, seed : 'S) =
            acc.Invoke(seed, x.Key)

        override x.Exists(predicate: 'K -> bool) =
            predicate x.Key
                
        override x.Forall(predicate: 'K -> bool) =
            predicate x.Key

        override x.CopyTo(dst : 'K array, index : ref<int>) =
            dst.[!index] <- x.Key
            index := !index + 1

        static member New(h: uint32, k: 'K) : AbstractNode<'K> = 
            new NoCollisionLeaf<'K>(Hash = h, Key = k) :> _

    and [<Sealed>] Node<'K> private() =
        inherit AbstractNode<'K>()
        [<DefaultValue>]
        val mutable public Prefix: uint32
        [<DefaultValue>]
        val mutable public Mask: Mask
        [<DefaultValue>]
        val mutable public Left: AbstractNode<'K>
        [<DefaultValue>]
        val mutable public Right: AbstractNode<'K>
        [<DefaultValue>]
        val mutable public _Count: int

        override x.Count = x._Count

        static member Join (p0 : uint32, t0 : AbstractNode<'K>, p1 : uint32, t1 : AbstractNode<'K>) : AbstractNode<'K>=
            let m = getMask p0 p1
            if zeroBit p0 m = 0u then Node<_>.New(getPrefix p0 m, m, t0, t1)
            else Node<_>.New(getPrefix p0 m, m, t1, t0)

        static member Create(p: uint32, m: Mask, l: AbstractNode<'K>, r: AbstractNode<'K>) =
            if r.IsEmpty then l
            elif l.IsEmpty then r
            else Node<_>.New(p, m, l, r)

        override x.ToArray(dst, o) =
            x.Left.ToArray(dst, o)
            x.Right.ToArray(dst, o)

        override x.IsEmpty = false
        
        override x.Accept(v: NodeVisitor<_,_>) =
            v.VisitNode x

        override x.Contains(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            #if OPTIMISTIC 
            let m = zeroBit hash x.Mask
            if m = 0u then x.Left.Contains(cmp, hash, key)
            else x.Right.Contains(cmp, hash, key)
            #else
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then x.Left.Contains(cmp, hash, key)
            elif m = 1u then x.Right.Contains(cmp, hash, key)
            else false
            #endif

        override x.Remove(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                let l = x.Left.Remove(cmp, hash, key)
                if l == x.Left then x :> _
                else Node<_>.Create(x.Prefix, x.Mask, l, x.Right)
            elif m = 1u then
                let r = x.Right.Remove(cmp, hash, key)
                if r == x.Right then x :> _
                else Node<_>.Create(x.Prefix, x.Mask, x.Left, r)
            else
                x:> _

        override x.TryRemove(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                match x.Left.TryRemove(cmp, hash, key) with
                | ValueSome ll ->
                    ValueSome (Node<_>.Create(x.Prefix, x.Mask, ll, x.Right))
                | ValueNone ->
                    ValueNone
            elif m = 1u then
                match x.Right.TryRemove(cmp, hash, key) with
                | ValueSome rr ->
                    ValueSome (Node<_>.Create(x.Prefix, x.Mask, x.Left, rr))
                | ValueNone ->
                    ValueNone
            else
                ValueNone

        override x.AddInPlaceUnsafe(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                x.Left <- x.Left.AddInPlaceUnsafe(cmp, hash, key)
                x._Count <- x.Left.Count + x.Right.Count
                x:> AbstractNode<_>
            elif m = 1u then 
                x.Right <- x.Right.AddInPlaceUnsafe(cmp, hash, key)
                x._Count <- x.Left.Count + x.Right.Count
                x:> AbstractNode<_>
            else
                let n = NoCollisionLeaf<_>.New(hash, key)
                Node<_>.Join(x.Prefix, x, hash, n)

        override x.Add(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                Node<_>.New(x.Prefix, x.Mask, x.Left.Add(cmp, hash, key), x.Right)
            elif m = 1u then 
                Node<_>.New(x.Prefix, x.Mask, x.Left, x.Right.Add(cmp, hash, key))
            else
                let n = NoCollisionLeaf<_>.New(hash, key)
                Node<_>.Join(x.Prefix, x, hash, n)

        override x.Alter(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, update: bool -> bool) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                let ll = x.Left.Alter(cmp, hash, key, update)
                if ll == x.Left then x:> _
                else Node<_>.New(x.Prefix, x.Mask, ll, x.Right)
            elif m = 1u then
                let rr = x.Right.Alter(cmp, hash, key, update)
                if rr == x.Right then x:> _
                else Node<_>.New(x.Prefix, x.Mask, x.Left, rr)
            else
                match update false with
                | false -> x:> _
                | true ->
                    let n = NoCollisionLeaf<_>.New(hash, key)
                    Node<_>.Join(x.Prefix, x, hash, n)
                    
        override x.Filter(predicate: 'K -> bool) =
            Node<_>.Create(x.Prefix, x.Mask, x.Left.Filter(predicate), x.Right.Filter(predicate))
            
        override x.Iter(action: 'K -> unit) =
            x.Left.Iter(action)
            x.Right.Iter(action)

        override x.Fold(acc: OptimizedClosures.FSharpFunc<'S, 'K, 'S>, seed : 'S) =
            let s = x.Left.Fold(acc, seed)
            x.Right.Fold(acc, s)
            

        override x.Exists(predicate: 'K -> bool) =
            x.Left.Exists predicate || x.Right.Exists predicate
                
        override x.Forall(predicate: 'K -> bool) =
            x.Left.Forall predicate && x.Right.Forall predicate

        override x.CopyTo(dst : 'K array, index : ref<int>) =
            x.Left.CopyTo(dst, index)
            x.Right.CopyTo(dst, index)

        static member New(p: uint32, m: Mask, l: AbstractNode<'K>, r: AbstractNode<'K>) : AbstractNode<'K> = 
            new Node<'K>(Prefix = p, Mask = m, Left = l, Right = r, _Count = l.Count + r.Count) :> _

    and [<AbstractClass>] NodeVisitor2<'K, 'R>() =
        abstract member VisitNN     : Node<'K> * Node<'K> -> 'R

        abstract member VisitNL     : Node<'K> * Leaf<'K> -> 'R
        abstract member VisitLN     : Leaf<'K> * Node<'K> -> 'R
        abstract member VisitLL     : Leaf<'K> * Leaf<'K> -> 'R

        abstract member VisitAE     : AbstractNode<'K> * Empty<'K> -> 'R
        abstract member VisitEA     : Empty<'K> * AbstractNode<'K> -> 'R
        abstract member VisitEE     : Empty<'K> * Empty<'K> -> 'R

    type Visit2Visitor<'K, 'R>(real : NodeVisitor2<'K, 'R>, node : AbstractNode<'K>) =
        inherit NodeVisitor<'K, 'R>()

        override x.VisitLeaf l = 
            node.Accept {
                new NodeVisitor<'K, 'R>() with
                    member x.VisitLeaf r = real.VisitLL(l, r)
                    member x.VisitNode r = real.VisitLN(l, r)
                    member x.VisitNoCollision r = real.VisitLL(l, r)
                    member x.VisitEmpty r = real.VisitAE(l, r)
            }
            
        override x.VisitNode l = 
            node.Accept {
                new NodeVisitor<'K, 'R>() with
                    member x.VisitLeaf r = real.VisitNL(l, r)
                    member x.VisitNode r = real.VisitNN(l, r)
                    member x.VisitNoCollision r = real.VisitNL(l, r)
                    member x.VisitEmpty r = real.VisitAE(l, r)
            }
            
        override x.VisitNoCollision l = 
            node.Accept {
                new NodeVisitor<'K, 'R>() with
                    member x.VisitLeaf r = real.VisitLL(l, r)
                    member x.VisitNode r = real.VisitLN(l, r)
                    member x.VisitNoCollision r = real.VisitLL(l, r)
                    member x.VisitEmpty r = real.VisitAE(l, r)
            }
            
        override x.VisitEmpty l = 
            node.Accept {
                new NodeVisitor<'K, 'R>() with
                    member x.VisitLeaf r = real.VisitEA(l, r)
                    member x.VisitNode r = real.VisitEA(l, r)
                    member x.VisitNoCollision r = real.VisitEA(l, r)
                    member x.VisitEmpty r = real.VisitEE(l, r)
            }

    module Visit2 = 
        let visit (v : NodeVisitor2<'K, 'R>) (l : AbstractNode<'K>) (r : AbstractNode<'K>) =
            l.Accept (Visit2Visitor<_,_>(v, r))

[<Struct>]
type HashSetOkasaki<'K> internal(cmp: EqualityComparer<'K>, root: AbstractNode<'K>) =

    static member Empty = HashSetOkasaki<'K>(EqualityComparer<'K>.Default, Empty<_>.Instance)

    member x.Count = root.Count
    member x.IsEmpty = root.IsEmpty

    member internal x.Root = root
    
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Single(key: 'K) =  
        let cmp = EqualityComparer<'K>.Default
        HashSetOkasaki(cmp, NoCollisionLeaf<_>.New(uint32 (cmp.GetHashCode key), key))
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfSeq(elements: seq<'K>) =  
        let cmp = EqualityComparer<'K>.Default
        let mutable r = Empty<'K>.Instance 
        for k in elements do
            let hash = cmp.GetHashCode k |> uint32
            r <- r.AddInPlaceUnsafe(cmp, hash, k)
        HashSetOkasaki<'K>(cmp, r)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfList(elements: list<'K>) =  
        let cmp = EqualityComparer<'K>.Default
        let mutable r = Empty<'K>.Instance 
        for k in elements do
            let hash = cmp.GetHashCode k |> uint32
            r <- r.AddInPlaceUnsafe(cmp, hash, k)
        HashSetOkasaki<'K>(cmp, r)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfListUnoptimized(elements: list<'K>) =  
        let cmp = EqualityComparer<'K>.Default
        let mutable r = Empty<'K>.Instance 
        for k in elements do
            let hash = cmp.GetHashCode k |> uint32
            r <- r.Add(cmp, hash, k)
        HashSetOkasaki<'K>(cmp, r)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfArray(elements: array<'K>) =  
        let cmp = EqualityComparer<'K>.Default
        let mutable r = Empty<'K>.Instance 
        for k in elements do
            let hash = cmp.GetHashCode k |> uint32
            r <- r.AddInPlaceUnsafe(cmp, hash, k)
        HashSetOkasaki<'K>(cmp, r)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Add(key: 'K) =
        let hash = cmp.GetHashCode key |> uint32
        let newRoot = root.Add(cmp, hash, key)
        HashSetOkasaki(cmp, newRoot)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Remove(key: 'K) =
        let hash = cmp.GetHashCode key |> uint32
        let newRoot = root.Remove(cmp, hash, key)
        HashSetOkasaki(cmp, newRoot)
         
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.TryRemove(key: 'K) =
        let hash = cmp.GetHashCode key |> uint32
        match root.TryRemove(cmp, hash, key) with
        | ValueSome newRoot ->
            Some (HashSetOkasaki(cmp, newRoot))
        | ValueNone ->
            None
         
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Contains(key: 'K) =
        let hash = cmp.GetHashCode key |> uint32
        root.Contains(cmp, hash, key)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Alter(key: 'K, update: bool -> bool) =
        let hash = cmp.GetHashCode key |> uint32
        let newRoot = root.Alter(cmp, hash, key, update)
        HashSetOkasaki(cmp, newRoot)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Filter(predicate: 'K -> bool) =
        let newRoot = root.Filter(predicate)
        HashSetOkasaki(cmp, newRoot)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Iter(action: 'K -> unit) =
        root.Iter(action)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Fold(acc: 'S -> 'K -> 'S, seed : 'S) =
        let acc = OptimizedClosures.FSharpFunc<'S, 'K, 'S>.Adapt acc
        root.Fold(acc, seed)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Exists(predicate: 'K -> bool) =
        root.Exists predicate
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Forall(predicate: 'K -> bool) =
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
        
    //[<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    //static member ComputeDelta(l : HashMapOkasaki<'K, 'V>, r : HashMapOkasaki<'K, 'V>, add : 'K -> 'V -> 'OP, update : 'K -> 'V -> 'V -> ValueOption<'OP>, remove : 'K -> 'V -> 'OP) =   
    //    let add = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(add)
    //    let remove = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(remove)
    //    let update = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(update)

        
    //    let len = ref 0
    //    let arr = ref (Array.zeroCreate 4)
    //    let cmp = EqualityComparer<'K>.Default

    //    let result = 
    //        let cnt = ()
    //        (l.Root, r.Root) ||> Visit2.visit {
    //            new NodeVisitor2<'K, 'V, 'V, AbstractNode<'K, 'OP>>() with

    //                member x.VisitEE(_, _) = Empty<'K, 'OP>.Instance
    //                member x.VisitEA(_, r) = r.Map(add)
    //                member x.VisitAE(l, _) = l.Map(remove)

    //                member x.VisitLL(l, r) = 
    //                    if l == r then
    //                        Empty<'K, 'OP>.Instance
    //                    else
    //                        len := 0
    //                        if l.LHash = r.LHash then
    //                            let mutable r = r :> AbstractNode<_,_>
    //                            let mutable res = Empty<'K, 'OP>.Instance
    //                            let hash = l.LHash
                        
    //                            l.ToArray(arr, len)
    //                            for i in 0 .. !len - 1 do
    //                                let struct (k, lv) = arr.Value.[i]
    //                                match r.TryRemove(cmp, hash, k) with
    //                                | ValueSome (rv, rest) ->
    //                                    r <- rest
    //                                    match update.Invoke(k, lv, rv) with
    //                                    | ValueSome op ->
    //                                        res <- res.AddInPlaceUnsafe(cmp, hash, k, op)
    //                                    | ValueNone ->
    //                                        ()
    //                                | ValueNone ->
    //                                    res <- res.AddInPlaceUnsafe(cmp, hash, k, remove.Invoke(k, lv))

    //                            len := 0
    //                            r.ToArray(arr, len)
    //                            for i in 0 .. !len - 1 do
    //                                let struct (k, rv) = arr.Value.[i]
    //                                res <- res.AddInPlaceUnsafe(cmp, hash, k, add.Invoke(k, rv))
                        
    //                            res
    //                        else
    //                            let mutable res = l.Map(remove)
    //                            r.ToArray(arr, len)
    //                            for i in 0 .. !len - 1 do
    //                                let struct (k, rv) = arr.Value.[i]
    //                                res <- res.AddInPlaceUnsafe(cmp, r.LHash, k, add.Invoke(k, rv))
    //                            res

    //                member x.VisitLN(l, r) =
    //                    let b = matchPrefixAndGetBit l.LHash r.Prefix r.Mask
    //                    if b = 0u then
    //                        Node.Create(r.Prefix, r.Mask, Visit2.visit x l r.Left, r.Right.Map(add))
    //                    elif b = 1u then
    //                        Node.Create(r.Prefix, r.Mask, r.Left.Map(add), Visit2.visit x l r.Right)
    //                    else
    //                        Node.Join(l.LHash, l.Map(remove), r.Prefix, r.Map(add))

    //                member x.VisitNL(l, r) =
    //                    let b = matchPrefixAndGetBit r.LHash l.Prefix l.Mask
    //                    if b = 0u then
    //                        Node.Create(l.Prefix, l.Mask, Visit2.visit x l.Left r, l.Right.Map(remove))
    //                    elif b = 1u then
    //                        Node.Create(l.Prefix, l.Mask, l.Left.Map(remove), Visit2.visit x l.Right r)
    //                    else
    //                        Node.Join(l.Prefix, l.Map(remove), r.LHash, r.Map(add))

    //                member x.VisitNN(l, r) = 
    //                    if l == r then
    //                        Empty<'K, 'OP>.Instance
    //                    else
    //                        let cc = compareMasks l.Mask r.Mask
    //                        if cc = 0 then
    //                            let l' = (l.Left, r.Left) ||> Visit2.visit x
    //                            let r' = (l.Right, r.Right) ||> Visit2.visit x
    //                            Node.Create(l.Prefix, l.Mask, l', r')
    //                        elif cc > 0 then
    //                            let lr = matchPrefixAndGetBit l.Prefix r.Prefix r.Mask
    //                            if lr = 0u then
    //                                Node.Create(r.Prefix, r.Mask, Visit2.visit x l r.Left, r.Right.Map(add))
    //                            elif lr = 1u then
    //                                Node.Create(r.Prefix, r.Mask, r.Left.Map(add), Visit2.visit x l r.Right)
    //                            else
    //                                Node.Join(l.Prefix, l.Map(remove), r.Prefix, r.Map(add))
    //                        else
    //                            let rl = matchPrefixAndGetBit r.Prefix l.Prefix l.Mask
                            
    //                            if rl = 0u then
    //                                Node.Create(l.Prefix, l.Mask, Visit2.visit x l.Left r, l.Right.Map(remove))
    //                            elif rl = 1u then
    //                                Node.Create(l.Prefix, l.Mask, l.Left.Map(remove), Visit2.visit x l.Right r)
    //                            else
    //                                Node.Join(l.Prefix, l.Map(remove), r.Prefix, r.Map(add))
                                    
    //        }

    //    HashMapOkasaki(cmp, result)
 
    //[<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    //static member UnionWith(l : HashMapOkasaki<'K, 'V>, r : HashMapOkasaki<'K, 'V>, resolve : 'K -> 'V -> 'V -> 'V) =   
    //    let update = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(resolve)

    //    let len = ref 0
    //    let arr = ref (Array.zeroCreate 4)
    //    let cmp = EqualityComparer<'K>.Default

    //    let result = 
    //        let cnt = ()
    //        (l.Root, r.Root) ||> Visit2.visit {
    //            new NodeVisitor2<'K, 'V, 'V, AbstractNode<'K, 'V>>() with

    //                member x.VisitEE(_, _) = Empty<'K, 'V>.Instance
    //                member x.VisitEA(_, r) = r
    //                member x.VisitAE(l, _) = l

    //                member x.VisitLL(l, r) = 
    //                    len := 0
    //                    if l.LHash = r.LHash then
    //                        let mutable r = r :> AbstractNode<_,_>
    //                        let mutable res = Empty<'K, 'V>.Instance
    //                        let hash = l.LHash
                    
    //                        l.ToArray(arr, len)
    //                        for i in 0 .. !len - 1 do
    //                            let struct (k, lv) = arr.Value.[i]
    //                            match r.TryRemove(cmp, hash, k) with
    //                            | ValueSome (rv, rest) ->
    //                                r <- rest
    //                                let op = update.Invoke(k, lv, rv)
    //                                res <- res.AddInPlaceUnsafe(cmp, hash, k, op)
    //                            | ValueNone ->
    //                                res <- res.AddInPlaceUnsafe(cmp, hash, k, lv)

    //                        len := 0
    //                        r.ToArray(arr, len)
    //                        for i in 0 .. !len - 1 do
    //                            let struct (k, rv) = arr.Value.[i]
    //                            res <- res.AddInPlaceUnsafe(cmp, hash, k, rv)
                    
    //                        res
    //                    else
    //                        Node.Join(l.LHash, l, r.LHash, r)
                         

    //                member x.VisitLN(l, r) =
    //                    let b = matchPrefixAndGetBit l.LHash r.Prefix r.Mask
    //                    if b = 0u then
    //                        Node.Create(r.Prefix, r.Mask, Visit2.visit x l r.Left, r.Right)
    //                    elif b = 1u then
    //                        Node.Create(r.Prefix, r.Mask, r.Left, Visit2.visit x l r.Right)
    //                    else
    //                        Node.Join(l.LHash, l, r.Prefix, r)

    //                member x.VisitNL(l, r) =
    //                    let b = matchPrefixAndGetBit r.LHash l.Prefix l.Mask
    //                    if b = 0u then
    //                        Node.Create(l.Prefix, l.Mask, Visit2.visit x l.Left r, l.Right)
    //                    elif b = 1u then
    //                        Node.Create(l.Prefix, l.Mask, l.Left, Visit2.visit x l.Right r)
    //                    else
    //                        Node.Join(l.Prefix, l, r.LHash, r)

    //                member x.VisitNN(l, r) = 
    //                    let cc = compareMasks l.Mask r.Mask
    //                    if cc = 0 then
    //                        let l' = (l.Left, r.Left) ||> Visit2.visit x
    //                        let r' = (l.Right, r.Right) ||> Visit2.visit x
    //                        Node.Create(l.Prefix, l.Mask, l', r')
    //                    elif cc > 0 then
    //                        let lr = matchPrefixAndGetBit l.Prefix r.Prefix r.Mask
    //                        if lr = 0u then
    //                            Node.Create(r.Prefix, r.Mask, Visit2.visit x l r.Left, r.Right)
    //                        elif lr = 1u then
    //                            Node.Create(r.Prefix, r.Mask, r.Left, Visit2.visit x l r.Right)
    //                        else
    //                            Node.Join(l.Prefix, l, r.Prefix, r)
    //                    else
    //                        let rl = matchPrefixAndGetBit r.Prefix l.Prefix l.Mask
                        
    //                        if rl = 0u then
    //                            Node.Create(l.Prefix, l.Mask, Visit2.visit x l.Left r, l.Right)
    //                        elif rl = 1u then
    //                            Node.Create(l.Prefix, l.Mask, l.Left, Visit2.visit x l.Right r)
    //                        else
    //                            Node.Join(l.Prefix, l, r.Prefix, r)
                                
    //        }

    //    HashMapOkasaki(cmp, result)
  
    //[<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    //static member Union(l : HashMapOkasaki<'K, 'V>, r : HashMapOkasaki<'K, 'V>) =   

    //    let len = ref 0
    //    let arr = ref (Array.zeroCreate 4)
    //    let cmp = EqualityComparer<'K>.Default

    //    let result = 
    //        (l.Root, r.Root) ||> Visit2.visit {
    //            new NodeVisitor2<'K, 'V, 'V, AbstractNode<'K, 'V>>() with

    //                member x.VisitEE(_, _) = Empty<'K, 'V>.Instance
    //                member x.VisitEA(_, r) = r
    //                member x.VisitAE(l, _) = l

    //                member x.VisitLL(l, r) = 
    //                    if l == r then
    //                        r :> AbstractNode<_,_>
    //                    else
    //                        len := 0
    //                        if l.LHash = r.LHash then
    //                            let mutable r = r :> AbstractNode<_,_>
    //                            let mutable res = Empty<'K, 'V>.Instance
    //                            let hash = l.LHash
                
    //                            l.ToArray(arr, len)
    //                            for i in 0 .. !len - 1 do
    //                                let struct (k, lv) = arr.Value.[i]
    //                                match r.TryRemove(cmp, hash, k) with
    //                                | ValueSome (rv, rest) ->
    //                                    r <- rest
    //                                    res <- res.AddInPlaceUnsafe(cmp, hash, k, rv)
    //                                | ValueNone ->
    //                                    res <- res.AddInPlaceUnsafe(cmp, hash, k, lv)

    //                            len := 0
    //                            r.ToArray(arr, len)
    //                            for i in 0 .. !len - 1 do
    //                                let struct (k, rv) = arr.Value.[i]
    //                                res <- res.AddInPlaceUnsafe(cmp, hash, k, rv)
                
    //                            res
    //                        else
    //                            Node.Join(l.LHash, l, r.LHash, r)
                     

    //                member x.VisitLN(l, r) =
    //                    let b = matchPrefixAndGetBit l.LHash r.Prefix r.Mask
    //                    if b = 0u then
    //                        Node.Create(r.Prefix, r.Mask, Visit2.visit x l r.Left, r.Right)
    //                    elif b = 1u then
    //                        Node.Create(r.Prefix, r.Mask, r.Left, Visit2.visit x l r.Right)
    //                    else
    //                        Node.Join(l.LHash, l, r.Prefix, r)

    //                member x.VisitNL(l, r) =
    //                    let b = matchPrefixAndGetBit r.LHash l.Prefix l.Mask
    //                    if b = 0u then
    //                        Node.Create(l.Prefix, l.Mask, Visit2.visit x l.Left r, l.Right)
    //                    elif b = 1u then
    //                        Node.Create(l.Prefix, l.Mask, l.Left, Visit2.visit x l.Right r)
    //                    else
    //                        Node.Join(l.Prefix, l, r.LHash, r)

    //                member x.VisitNN(l, r) = 
    //                    if l == r then 
    //                        r :> AbstractNode<_,_>
    //                    else
    //                        let cc = compareMasks l.Mask r.Mask
    //                        if cc = 0 then
    //                            let l' = (l.Left, r.Left) ||> Visit2.visit x
    //                            let r' = (l.Right, r.Right) ||> Visit2.visit x
    //                            Node.Create(l.Prefix, l.Mask, l', r')
    //                        elif cc > 0 then
    //                            let lr = matchPrefixAndGetBit l.Prefix r.Prefix r.Mask
    //                            if lr = 0u then
    //                                Node.Create(r.Prefix, r.Mask, Visit2.visit x l r.Left, r.Right)
    //                            elif lr = 1u then
    //                                Node.Create(r.Prefix, r.Mask, r.Left, Visit2.visit x l r.Right)
    //                            else
    //                                Node.Join(l.Prefix, l, r.Prefix, r)
    //                        else
    //                            let rl = matchPrefixAndGetBit r.Prefix l.Prefix l.Mask
                    
    //                            if rl = 0u then
    //                                Node.Create(l.Prefix, l.Mask, Visit2.visit x l.Left r, l.Right)
    //                            elif rl = 1u then
    //                                Node.Create(l.Prefix, l.Mask, l.Left, Visit2.visit x l.Right r)
    //                            else
    //                                Node.Join(l.Prefix, l, r.Prefix, r)
                            
    //        }

    //    HashMapOkasaki(cmp, result)
  

    //[<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    //static member ApplyDelta(state : HashMapOkasaki<'K, 'V>, delta : HashMapOkasaki<'K, 'D>, apply : 'K -> voption<'V> -> 'D -> struct(voption<'V> * voption<'D>)) =   
    //    let apply = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(apply)

    //    let onlyDelta = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(fun k d -> apply.Invoke(k, ValueNone, d))
    
    //    let len = ref 0
    //    let arr1 = ref (Array.zeroCreate 4)
    //    let arr2 = ref (Array.zeroCreate 4)
    //    let cmp = EqualityComparer<'K>.Default

    //    let struct(result, delta) = 
    //        let cnt = ()
    //        (state.Root, delta.Root) ||> Visit2.visit {
    //            new NodeVisitor2<'K, 'V, 'D, struct(AbstractNode<'K, 'V> * AbstractNode<'K, 'D>)>() with

    //                member x.VisitEE(_, _) = 
    //                    struct (Empty.Instance, Empty.Instance)

    //                member x.VisitEA(_, r) =    
    //                    r.ChooseV2 onlyDelta

    //                member x.VisitAE(l, _) = 
    //                    struct(l, Empty.Instance)

    //                member x.VisitLL(state, delta) = 
    //                    len := 0
    //                    if state.LHash = delta.LHash then
    //                        let mutable delta = delta :> AbstractNode<_,_>
    //                        let mutable resState = Empty<'K, 'V>.Instance
    //                        let mutable resDelta = Empty<'K, 'D>.Instance
    //                        let hash = state.LHash
                    
    //                        state.ToArray(arr1, len)
    //                        for i in 0 .. !len - 1 do
    //                            let struct (k, value) = arr1.Value.[i]
    //                            match delta.TryRemove(cmp, hash, k) with
    //                            | ValueSome (dd, rest) ->
    //                                delta <- rest
    //                                let struct (s, d) = apply.Invoke(k, ValueSome value, dd)

    //                                match s with
    //                                | ValueSome v -> resState <- resState.AddInPlaceUnsafe(cmp, hash, k, v)
    //                                | ValueNone -> ()

    //                                match d with
    //                                | ValueSome v -> resDelta <- resDelta.AddInPlaceUnsafe(cmp, hash, k, v)
    //                                | ValueNone -> ()

    //                            | ValueNone ->
    //                                resState <- resState.AddInPlaceUnsafe(cmp, hash, k, value)

    //                        len := 0
    //                        delta.ToArray(arr2, len)
    //                        for i in 0 .. !len - 1 do
    //                            let struct (k, rv) = arr2.Value.[i]
    //                            let struct (s, d) = onlyDelta.Invoke(k, rv)
    //                            match s with
    //                            | ValueSome v -> resState <- resState.AddInPlaceUnsafe(cmp, hash, k, v)
    //                            | ValueNone -> ()
    //                            match d with
    //                            | ValueSome v -> resDelta <- resDelta.AddInPlaceUnsafe(cmp, hash, k, v)
    //                            | ValueNone -> ()
                    
    //                        struct(resState, resDelta)
    //                    else
    //                        let struct (ds, dd) = delta.ChooseV2(onlyDelta)
    //                        struct (
    //                            Node.Join(state.LHash, state, delta.LHash, ds),
    //                            dd
    //                        )

    //                member x.VisitLN(state, delta) =
    //                    let b = matchPrefixAndGetBit state.LHash delta.Prefix delta.Mask
    //                    if b = 0u then
    //                        let struct (ls, ld) = Visit2.visit x state delta.Left
    //                        let struct (rs, rd) = delta.Right.ChooseV2(onlyDelta)
    //                        struct(
    //                            Node.Create(delta.Prefix, delta.Mask, ls, rs),
    //                            Node.Create(delta.Prefix, delta.Mask, ld, rd)
    //                        )
    //                    elif b = 1u then
    //                        let struct (ls, ld) = delta.Left.ChooseV2(onlyDelta)
    //                        let struct (rs, rd) = Visit2.visit x state delta.Right
    //                        struct(
    //                            Node.Create(delta.Prefix, delta.Mask, ls, rs),
    //                            Node.Create(delta.Prefix, delta.Mask, ld, rd)
    //                        )
    //                    else
    //                        let struct (ds, dd) = delta.ChooseV2(onlyDelta)
    //                        struct(
    //                            Node.Join(state.LHash, state, delta.Prefix, ds),
    //                            dd
    //                        )

    //                member x.VisitNL(l, r) =
    //                    let b = matchPrefixAndGetBit r.LHash l.Prefix l.Mask
    //                    if b = 0u then
    //                        let struct (ls, ld) = Visit2.visit x l.Left r
    //                        struct (
    //                            Node.Create(l.Prefix, l.Mask, ls, l.Right),
    //                            ld
    //                        )
    //                    elif b = 1u then
    //                        let struct (rs, rd) = Visit2.visit x l.Right r
    //                        struct (
    //                            Node.Create(l.Prefix, l.Mask, l.Left, rs),
    //                            rd
    //                        )
    //                    else
    //                        let struct (rs, rd) = r.ChooseV2(onlyDelta)
    //                        struct (
    //                            Node.Join(l.Prefix, l, r.LHash, rs),
    //                            rd
    //                        )

    //                member x.VisitNN(l, r) = 
    //                    let cc = compareMasks l.Mask r.Mask
    //                    if cc = 0 then
    //                        let struct (ls, ld) = (l.Left, r.Left) ||> Visit2.visit x
    //                        let struct (rs, rd) = (l.Right, r.Right) ||> Visit2.visit x
    //                        struct (
    //                            Node.Create(l.Prefix, l.Mask, ls, rs),
    //                            Node.Create(l.Prefix, l.Mask, ld, rd)
    //                        )
    //                    elif cc > 0 then
    //                        let lr = matchPrefixAndGetBit l.Prefix r.Prefix r.Mask
    //                        if lr = 0u then
    //                            let struct (ls, ld) = Visit2.visit x l r.Left
    //                            let struct (rs, rd) = r.Right.ChooseV2(onlyDelta)
    //                            struct (
    //                                Node.Create(r.Prefix, r.Mask, ls, rs),
    //                                Node.Create(r.Prefix, r.Mask, ld, rd)
    //                            )
    //                        elif lr = 1u then
    //                            let struct (ls, ld) = r.Left.ChooseV2(onlyDelta)
    //                            let struct (rs, rd) = Visit2.visit x l r.Right
    //                            struct (
    //                                Node.Create(r.Prefix, r.Mask, ls, rs),
    //                                Node.Create(r.Prefix, r.Mask, ld, rd)
    //                            )
    //                        else
    //                            let struct (rs, rd) = r.ChooseV2 onlyDelta
    //                            struct (
    //                                Node.Join(l.Prefix, l, r.Prefix, rs),
    //                                rd
    //                            )
    //                    else
    //                        let rl = matchPrefixAndGetBit r.Prefix l.Prefix l.Mask
                        
    //                        if rl = 0u then
    //                            let struct (ls, ld) = Visit2.visit x l.Left r
    //                            struct (
    //                                Node.Create(l.Prefix, l.Mask, ls, l.Right),
    //                                ld
    //                            )
    //                        elif rl = 1u then
    //                            let struct (rs, rd) = Visit2.visit x l.Right r
    //                            struct (
    //                                Node.Create(l.Prefix, l.Mask, l.Left, rs),
    //                                rd
    //                            )
    //                        else
    //                            let struct (rs, rd) = r.ChooseV2 onlyDelta
    //                            struct (
    //                                Node.Join(l.Prefix, l, r.Prefix, rs),
    //                                rd
    //                            )
                                
    //        }

    //    HashMapOkasaki(cmp, result), HashMapOkasaki(cmp, delta)
 

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
        member x.GetEnumerator() = new HashSetOkasakiEnumerator<_>(root) :> _
        
    interface System.Collections.Generic.IEnumerable<'K> with 
        member x.GetEnumerator() = new HashSetOkasakiEnumerator<_>(root) :> _

and internal HashSetOkasakiEnumerator<'K>(root: AbstractNode<'K>) =
    let mutable stack = [root]
    let mutable linked: Linked<'K> = null
    let mutable current = Unchecked.defaultof<'K>

    member x.MoveNext() =
        if isNull linked then
            match stack with
            | (:? Empty<'K>) :: rest ->
                stack <- rest 
                x.MoveNext()
            | (:? NoCollisionLeaf<'K> as l) :: rest ->
                stack <- rest
                current <- l.Key
                true
            | (:? CollisionLeaf<'K> as l) :: rest -> 
                stack <- rest
                current <- l.Key
                linked <- l.Next
                true
            | (:? Node<'K> as n) :: rest ->
                stack <- n.Left:: n.Right:: rest
                x.MoveNext()
            | _ ->
                false
        else
            current <- linked.Key
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
        
    interface System.Collections.Generic.IEnumerator<'K> with
        member x.Dispose() = x.Dispose()
        member x.Current = x.Current

module HashSetOkasaki =

    /// The empty set.
    [<GeneralizableValue>]
    let empty<'K> = HashSetOkasaki<'K>.Empty

    /// The number of elements in the set `O(1)`
    let inline count (map: HashSetOkasaki<'K>) = map.Count
    
    /// Is the set empty? `O(1)`
    let inline isEmpty (map: HashSetOkasaki<'K>) = map.IsEmpty

    /// Creates a set with a single entry.
    /// `O(1)`
    let inline single (key: 'K) =
        HashSetOkasaki<'K>.Single(key)

    /// Creates a set with all entries from the seq.
    /// `O(N * log N)`
    let inline ofSeq (seq: seq<'K>) =
        HashSetOkasaki<'K>.OfSeq seq

    /// Creates a set with all entries from the given one.
    /// `O(N * log N)`
    let inline ofSet (map: Set<'K>) = 
        map |> Set.toSeq |> ofSeq

    /// Creates a set with all entries from the list.
    /// `O(N * log N)`
    let inline ofList (list: list<'K>) = 
        HashSetOkasaki<'K>.OfList list

    /// Creates a set with all entries from the array.
    /// `O(N * log N)`
    let inline ofArray (arr: array<'K>) = 
        HashSetOkasaki<'K>.OfArray arr

    /// Creates a set with all entries from the list.
    /// `O(N * log N)`
    let inline ofListUnoptimized (list: list<'K>) = 
        HashSetOkasaki<'K>.OfListUnoptimized list

    /// Creates a seq holding all values contained in the set.
    /// `O(N)`
    let inline toSeq (map: HashSetOkasaki<'K>) = 
        map.ToSeq()

    /// Creates a list holding all values contained in the set.
    /// `O(N)`
    let inline toList (map: HashSetOkasaki<'K>) = 
        map.ToList()

    /// Creates an array holding all values contained in the set.
    /// `O(N)`
    let inline toArray (map: HashSetOkasaki<'K>) = 
        map.ToArray()

    /// Creates a Set holding all entries contained in the HashSet.
    /// `O(N)`
    let inline toSet (map: HashSetOkasaki<'K>) =
        map.ToSeq() |> Set.ofSeq

    /// Adds the given value. `O(log N)`
    let inline add (key: 'K) (map: HashSetOkasaki<'K>) =
        map.Add(key)

    /// Removes the given key. `O(log N)`
    let inline remove (key: 'K) (map: HashSetOkasaki<'K>) =
        map.Remove(key)
 
    /// Tries to remove the given key from the set and (optionally) returns the rest of the set.
    /// `O(log N)`       
    let inline tryRemove (key: 'K) (map: HashSetOkasaki<'K>) =
        map.TryRemove(key)

    /// Tests if the given key is part of the set. `O(log N)`
    let inline contains (key: 'K) (map: HashSetOkasaki<'K>) =
        map.Contains(key)

    /// Adds, deletes or updates the entry for the given key.
    /// The update functions gets the a boolean indicating whether the value existed
    //// and may return a boolean  indicating whether the value should exist in the output.
    /// `O(log N)`
    let inline alter (key: 'K) (update: bool -> bool) (map: HashSetOkasaki<'K>) =
        map.Alter(key, update)
    
    /// Creates a new set by applying the given function to all entries.
    /// `O(N log N)`
    let map (mapping: 'K -> 'T) (map: HashSetOkasaki<'K>) =
        let cmp = EqualityComparer<'T>.Default
        let mutable res = Empty<_>.Instance
        for k in map do
            let v = mapping k
            let hash = cmp.GetHashCode v
            res <- res.AddInPlaceUnsafe(cmp, uint32 hash, v)
        HashSetOkasaki(cmp, res)
    
    /// Creates a new set by applying the given function to all entries.
    /// `O(N log N)`
    let choose (mapping: 'K -> option<'T>) (map: HashSetOkasaki<'K>) =
        let cmp = EqualityComparer<'T>.Default
        let mutable res = Empty<_>.Instance
        for k in map do
            match mapping k with
            | Some v ->
                let hash = cmp.GetHashCode v
                res <- res.AddInPlaceUnsafe(cmp, uint32 hash, v)
            | None ->
                ()
        HashSetOkasaki(cmp, res)
    
    /// Creates a new set (with the same keys) that contains all entries for which predicate was true.
    /// `O(N)`
    let inline filter (predicate: 'K -> bool) (map: HashSetOkasaki<'K>) =
        map.Filter predicate

    /// Applies the iter function to all entries of the set.
    /// `O(N)`
    let inline iter (iter: 'K -> unit) (map: HashSetOkasaki<'K>) =
        map.Iter iter

    /// Folds over all entries of the set.
    /// Note that the order for elements is undefined.
    /// `O(N)`
    let inline fold (folder: 'State -> 'K -> 'State) (seed: 'State) (map: HashSetOkasaki<'K>) =
        map.Fold(folder, seed)
        
    /// Tests whether an entry making the predicate true exists.
    /// `O(N)`
    let inline exists (predicate: 'K -> bool) (map: HashSetOkasaki<'K>) =
        map.Exists(predicate)

    /// Tests whether all entries fulfil the given predicate.
    /// `O(N)`
    let inline forall (predicate: 'K -> bool) (map: HashSetOkasaki<'K>) =
        map.Forall(predicate)
