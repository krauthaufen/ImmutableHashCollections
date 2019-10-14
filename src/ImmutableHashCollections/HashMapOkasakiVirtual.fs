namespace ImmutableHashCollections

open System.Collections
open System.Collections.Generic
#if NETCOREAPP3_0 
open System.Runtime.Intrinsics.X86
open System.Runtime.CompilerServices

#endif


[<AutoOpen>]
module internal HashMapOkasakiVirtualImplementation = 
    let inline mask (k : uint32) (m : uint32) = 
        #if NETCOREAPP3_0 
        Bmi1.AndNot((m <<< 1) - 1u, k)
        #else
        //k &&& (m - 1u) // little endian
        //(k ||| (m - 1u)) &&& ~~~m // big endian
        k &&& ~~~((m <<< 1) - 1u)
        #endif
    let inline matchPrefix (k : uint32) (p : uint32) (m : uint32) =
        mask k m = p

    let inline zeroBit (k : uint32) (m : uint32) =
        (k &&& m) = 0u

    let inline (==) (a : ^a) (b : ^a) =
        System.Object.ReferenceEquals(a, b)

    let inline highestBitMask x =
        #if NETCOREAPP3_0
        0x80000000u >>> int (Lzcnt.LeadingZeroCount x)
        #else
        let mutable x = x
        x <- x ||| (x >>> 1)
        x <- x ||| (x >>> 2)
        x <- x ||| (x >>> 4)
        x <- x ||| (x >>> 8)
        x <- x ||| (x >>> 16)
        x ^^^ (x >>> 1)
        #endif

    let inline lowestBitMask x =
        x &&& (~~~x + 1u)

    let inline branchingBit (p0 : uint32) (p1 : uint32) =
        //lowestBitMask (p0 ^^^ p1) // little endian
        highestBitMask (p0 ^^^ p1) // big endian

    [<AllowNullLiteral>]
    type Linked<'K, 'V> =
        val mutable public Next : Linked<'K, 'V>
        val mutable public Key : 'K
        val mutable public Value : 'V

        new(k, v) = { Key = k; Value = v; Next = null }
        new(k, v, n) = { Key = k; Value = v; Next = n }

    module Linked =
    
        let rec addInPlaceUnsafe (cmp : IEqualityComparer<'K>) (cnt : ref<int>) (key : 'K) (value : 'V) (n : Linked<'K, 'V>) =
            if isNull n then
                cnt := !cnt + 1
                Linked(key, value)
            elif cmp.Equals(n.Key, key) then
                n.Key <- key
                n.Value <- value
                n
            else
                n.Next <- addInPlaceUnsafe cmp cnt key value n.Next
                n

        let rec add (cmp : IEqualityComparer<'K>) (cnt : ref<int>) (key : 'K) (value : 'V) (n : Linked<'K, 'V>) =
            if isNull n then
                cnt := !cnt + 1
                Linked(key, value)
            elif cmp.Equals(n.Key, key) then
                Linked(key, value, n.Next)
            else
                Linked(n.Key, n.Value, add cmp cnt key value n.Next)
               
        let rec alter (cmp : IEqualityComparer<'K>) (cnt : ref<int>) (key : 'K) (update : option<'V> -> option<'V>) (n : Linked<'K, 'V>) =
            if isNull n then
                match update None with
                | Some value -> 
                    cnt := !cnt + 1
                    Linked(key, value)
                | None ->
                    null
            elif cmp.Equals(n.Key, key) then
                match update (Some n.Value) with
                | Some value -> 
                    Linked(key, value, n.Next)
                | None -> 
                    cnt := !cnt - 1
                    n.Next
            else
                Linked(n.Key, n.Value, alter cmp cnt key update n.Next)
               
        let rec tryFind (cmp : IEqualityComparer<'K>) (key : 'K) (n : Linked<'K, 'V>) =
            if isNull n then None
            elif cmp.Equals(n.Key, key) then Some n.Value
            else tryFind cmp key n.Next

        let destruct (n : Linked<'K, 'V>) =
            if isNull n then ValueNone
            else ValueSome(struct (n.Key, n.Value, n.Next))
            
        let rec remove (cmp : IEqualityComparer<'K>) (cnt : ref<int>) (key : 'K) (n : Linked<'K, 'V>) =
            if isNull n then
                null
            elif cmp.Equals(n.Key, key) then 
                cnt := !cnt - 1
                n.Next
            else
                let rest = remove cmp cnt key n.Next
                if rest == n.Next then n
                else Linked(n.Key, n.Value, rest)

        let rec tryRemove (cmp : IEqualityComparer<'K>) (cnt : ref<int>) (key : 'K) (n : Linked<'K, 'V>) =
            if isNull n then
                ValueNone
            elif cmp.Equals(n.Key, key) then 
                cnt := !cnt - 1
                ValueSome (struct(n.Value, n.Next))
            else
                match tryRemove cmp cnt key n.Next with
                | ValueSome (struct (value, rest)) ->
                    ValueSome(struct(value, Linked(n.Key, n.Value, rest)))
                | ValueNone ->
                    ValueNone

    [<AbstractClass>]
    type AbstractNode<'K, 'V>() =
        abstract member Remove : IEqualityComparer<'K> * ref<int> * uint32 * 'K -> AbstractNode<'K, 'V>
        abstract member TryRemove : IEqualityComparer<'K> * ref<int> * uint32 * 'K -> ValueOption<struct ('V * AbstractNode<'K, 'V>)>

        abstract member AddInPlaceUnsafe : IEqualityComparer<'K> * ref<int> * uint32 * 'K * 'V -> AbstractNode<'K, 'V>
        abstract member Add : IEqualityComparer<'K> * ref<int> * uint32 * 'K * 'V -> AbstractNode<'K, 'V>
        abstract member Alter : IEqualityComparer<'K> * ref<int> * uint32 * 'K * (option<'V> -> option<'V>) -> AbstractNode<'K, 'V>
        abstract member TryFind : IEqualityComparer<'K> * uint32 * 'K -> option<'V>
        abstract member IsEmpty : bool

    type Empty<'K, 'V> private() =
        inherit AbstractNode<'K, 'V>()
        static let instance = Empty<'K, 'V>() :> AbstractNode<_,_>
        static member Instance = instance

        override x.IsEmpty = true

        override x.TryFind(_cmp : IEqualityComparer<'K>, _hash : uint32, _key : 'K) =
            None

        override x.Remove(_cmp : IEqualityComparer<'K>, _cnt : ref<int>, _hash : uint32, _key : 'K) =
            x :> _
            
        override x.TryRemove(_cmp : IEqualityComparer<'K>, _cnt : ref<int>, _hash : uint32, _key : 'K) =
            ValueNone

        override x.AddInPlaceUnsafe(_cmp : IEqualityComparer<'K>, cnt : ref<int>, hash : uint32, key : 'K, value : 'V) =
            cnt := !cnt + 1
            NoCollisionLeaf<'K, 'V>(hash, key, value) :> _

        override x.Add(_cmp : IEqualityComparer<'K>, cnt : ref<int>, hash : uint32, key : 'K, value : 'V) =
            cnt := !cnt + 1
            NoCollisionLeaf<'K, 'V>(hash, key, value) :> _

        override x.Alter(cmp : IEqualityComparer<'K>, cnt : ref<int>, hash : uint32, key : 'K, update : option<'V> -> option<'V>) =
            match update None with
            | None -> x :> _
            | Some value ->
                cnt := !cnt + 1
                NoCollisionLeaf<'K, 'V>(hash, key, value) :> _



    and Leaf<'K, 'V> =
        inherit AbstractNode<'K, 'V>
        val mutable public Next : Linked<'K, 'V>
        val mutable public Key : 'K
        val mutable public Value : 'V
        val mutable public Hash : uint32
        
        static member Create(hash : uint32, key : 'K, value : 'V, next : Linked<'K, 'V>) =
            if isNull next then NoCollisionLeaf(hash, key, value) :> AbstractNode<'K, 'V>
            else Leaf(hash, key, value, next) :> AbstractNode<'K, 'V>

        override x.IsEmpty = false
        
        override x.TryFind(cmp : IEqualityComparer<'K>, hash : uint32, key : 'K) =   
            if hash = x.Hash then
                if cmp.Equals(key, x.Key) then 
                    Some x.Value
                else
                    Linked.tryFind cmp key x.Next
            else
                None

        override x.Remove(cmp : IEqualityComparer<'K>, cnt : ref<int>, hash : uint32, key : 'K) =
            if hash = x.Hash then
                if cmp.Equals(key, x.Key) then
                    cnt := !cnt - 1
                    match Linked.destruct x.Next with
                    | ValueSome (struct (k, v, rest)) ->
                        Leaf.Create(hash, k, v, rest)
                    | ValueNone ->
                        Empty<'K, 'V>.Instance
                else
                    Leaf.Create(x.Hash, x.Key, x.Value, Linked.remove cmp cnt key x.Next)
            else
                x :> _

        override x.TryRemove(cmp : IEqualityComparer<'K>, cnt : ref<int>, hash : uint32, key : 'K)         =
            if hash = x.Hash then
                if cmp.Equals(key, x.Key) then
                    cnt := !cnt - 1
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

        override x.AddInPlaceUnsafe(cmp : IEqualityComparer<'K>, cnt : ref<int>, hash : uint32, key : 'K, value : 'V) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    x.Key <- key
                    x.Value <- value
                    x :> _
                else
                    x.Next <- Linked.addInPlaceUnsafe cmp cnt key value x.Next
                    x :> _
            else
                cnt := !cnt + 1
                let n = NoCollisionLeaf<'K, 'V>(hash, key, value)
                Node.Join(hash, n, x.Hash, x)
                
        override x.Add(cmp : IEqualityComparer<'K>, cnt : ref<int>, hash : uint32, key : 'K, value : 'V) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    Leaf<'K, 'V>(x.Hash, key, value, x.Next) :> _
                else
                    Leaf<'K, 'V>(x.Hash, x.Key, x.Value, Linked.add cmp cnt key value x.Next) :> _
            else
                cnt := !cnt + 1
                let n = NoCollisionLeaf<'K, 'V>(hash, key, value)
                Node.Join(hash, n, x.Hash, x)

        override x.Alter(cmp : IEqualityComparer<'K>, cnt : ref<int>, hash : uint32, key : 'K, update : option<'V> -> option<'V>) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    match update (Some x.Value) with
                    | None ->
                        // remove
                        cnt := !cnt - 1
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
                    if n == x.Next then x :> _
                    else Leaf(x.Hash, x.Key, x.Value, n) :> _
            else
                // other hash => not contained
                match update None with
                | None -> x :> _
                | Some value ->
                    // add
                    cnt := !cnt + 1
                    let n = NoCollisionLeaf<'K, 'V>(hash, key, value)
                    Node.Join(hash, n, x.Hash, x)

        new(h : uint32, k : 'K, v : 'V, n : Linked<'K, 'V>) = { inherit AbstractNode<'K, 'V>(); Hash = h; Key = k; Value = v; Next = n }
     
    and NoCollisionLeaf<'K, 'V> =
        inherit AbstractNode<'K, 'V>
        val mutable public Key : 'K
        val mutable public Value : 'V
        val mutable public Hash : uint32
        
        override x.IsEmpty = false
        
        override x.TryFind(cmp : IEqualityComparer<'K>, hash : uint32, key : 'K) =   
            if hash = x.Hash && cmp.Equals(key, x.Key) then 
                Some x.Value
            else
                None

        override x.Remove(cmp : IEqualityComparer<'K>, cnt : ref<int>, hash : uint32, key : 'K) =
            if hash = x.Hash && cmp.Equals(key, x.Key) then
                cnt := !cnt - 1
                Empty<'K, 'V>.Instance
            else
                x :> _

        override x.TryRemove(cmp : IEqualityComparer<'K>, cnt : ref<int>, hash : uint32, key : 'K) =
            if hash = x.Hash && cmp.Equals(key, x.Key) then
                cnt := !cnt - 1
                ValueSome (struct(x.Value, Empty<'K, 'V>.Instance))
            else
                ValueNone

        override x.AddInPlaceUnsafe(cmp : IEqualityComparer<'K>, cnt : ref<int>, hash : uint32, key : 'K, value : 'V) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    x.Key <- key
                    x.Value <- value
                    x :> _
                else
                    cnt := !cnt + 1
                    Leaf<'K, 'V>(x.Hash, x.Key, x.Value, Linked(key, value, null)) :> _
            else
                cnt := !cnt + 1
                let n = NoCollisionLeaf<'K, 'V>(hash, key, value)
                Node.Join(hash, n, x.Hash, x)

        override x.Add(cmp : IEqualityComparer<'K>, cnt : ref<int>, hash : uint32, key : 'K, value : 'V) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    NoCollisionLeaf<'K, 'V>(x.Hash, key, value) :> _
                else
                    Leaf<'K, 'V>(x.Hash, x.Key, x.Value, Linked.add cmp cnt key value null) :> _
            else
                cnt := !cnt + 1
                let n = NoCollisionLeaf<'K, 'V>(hash, key, value)
                Node.Join(hash, n, x.Hash, x)
        
        override x.Alter(cmp : IEqualityComparer<'K>, cnt : ref<int>, hash : uint32, key : 'K, update : option<'V> -> option<'V>) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    match update (Some x.Value) with
                    | Some value -> 
                        NoCollisionLeaf(x.Hash, x.Key, value) :> _
                    | None -> 
                        cnt := !cnt - 1
                        Empty.Instance
                else
                    match update None with
                    | None -> x :> _
                    | Some value ->
                        cnt := !cnt + 1
                        Leaf(x.Hash, x.Key, x.Value, Linked(key, value, null)) :> _
            else
                match update None with
                | None -> x :> _
                | Some value ->
                    cnt := !cnt + 1
                    let n = NoCollisionLeaf<'K, 'V>(hash, key, value)
                    Node.Join(hash, n, x.Hash, x)
           
        new(h : uint32, k : 'K, v : 'V) = { inherit AbstractNode<'K, 'V>(); Hash = h; Key = k; Value = v }

    and Node<'K, 'V> =
        inherit AbstractNode<'K, 'V>
        val mutable public Prefix : uint32
        val mutable public Mask : uint32
        val mutable public Left : AbstractNode<'K, 'V>
        val mutable public Right : AbstractNode<'K, 'V>
        
        static member Join (p0 : uint32, t0 : AbstractNode<'K, 'V>, p1 : uint32, t1 : AbstractNode<'K, 'V>) : AbstractNode<'K,'V>=
            let m = branchingBit p0 p1
            if zeroBit p0 m then Node(mask p0 m, m, t0, t1) :> AbstractNode<_,_>
            else Node(mask p0 m, m, t1, t0) :> AbstractNode<_,_>

        static member Create(p : uint32, m : uint32, l : AbstractNode<'K, 'V>, r : AbstractNode<'K, 'V>) =
            if r.IsEmpty then l
            elif l.IsEmpty then r
            else Node(p, m, l, r) :> _
            
        override x.IsEmpty = false
        
        override x.TryFind(cmp : IEqualityComparer<'K>, hash : uint32, key : 'K) =
            if matchPrefix hash x.Prefix x.Mask then
                if zeroBit hash x.Mask then 
                    x.Left.TryFind(cmp, hash, key)
                else 
                    x.Right.TryFind(cmp, hash, key)
            else
                None
        override x.Remove(cmp : IEqualityComparer<'K>, cnt : ref<int>, hash : uint32, key : 'K) =
            if matchPrefix hash x.Prefix x.Mask then
                if zeroBit hash x.Mask then 
                    Node.Create(x.Prefix, x.Mask, x.Left.Remove(cmp, cnt, hash, key), x.Right)
                else 
                    Node.Create(x.Prefix, x.Mask, x.Left, x.Right.Remove(cmp, cnt, hash, key))
            else
                x :> _

        override x.TryRemove(cmp : IEqualityComparer<'K>, cnt : ref<int>, hash : uint32, key : 'K) =
            if matchPrefix hash x.Prefix x.Mask then
                if zeroBit hash x.Mask then 
                    match x.Left.TryRemove(cmp, cnt, hash, key) with
                    | ValueSome (struct(value, ll)) ->
                        ValueSome (struct(value, Node.Create(x.Prefix, x.Mask, ll, x.Right)))
                    | ValueNone ->
                        ValueNone
                else
                    match x.Right.TryRemove(cmp, cnt, hash, key) with
                    | ValueSome (struct(value, rr)) ->
                        ValueSome (struct(value, Node.Create(x.Prefix, x.Mask, x.Left, rr)))
                    | ValueNone ->
                        ValueNone
            else
                ValueNone

        override x.AddInPlaceUnsafe(cmp : IEqualityComparer<'K>, cnt : ref<int>, hash : uint32, key : 'K, value : 'V) =
            if matchPrefix hash x.Prefix x.Mask then
                if zeroBit hash x.Mask then 
                    x.Left <- x.Left.AddInPlaceUnsafe(cmp, cnt, hash, key, value)
                    x :> _
                else 
                    x.Right <- x.Right.AddInPlaceUnsafe(cmp, cnt, hash, key, value)
                    x :> _
            else
                cnt := !cnt + 1
                Node.Join(x.Prefix, x, hash, NoCollisionLeaf(hash, key, value))

        override x.Add(cmp : IEqualityComparer<'K>, cnt : ref<int>, hash : uint32, key : 'K, value : 'V) =
            if matchPrefix hash x.Prefix x.Mask then
                if zeroBit hash x.Mask then 
                    Node(x.Prefix, x.Mask, x.Left.Add(cmp, cnt, hash, key, value), x.Right) :> _
                else 
                    Node(x.Prefix, x.Mask, x.Left, x.Right.Add(cmp, cnt, hash, key, value)) :> _
            else
                cnt := !cnt + 1
                Node.Join(x.Prefix, x, hash, NoCollisionLeaf(hash, key, value))

        override x.Alter(cmp : IEqualityComparer<'K>, cnt : ref<int>, hash : uint32, key : 'K, update : option<'V> -> option<'V>) =
            if matchPrefix hash x.Prefix x.Mask then
                if zeroBit hash x.Mask then 
                    let ll = x.Left.Alter(cmp, cnt, hash, key, update)
                    if ll == x.Left then x :> _
                    else Node(x.Prefix, x.Mask, ll, x.Right) :> _
                else
                    let rr = x.Right.Alter(cmp, cnt, hash, key, update)
                    if rr == x.Right then x :> _
                    else Node(x.Prefix, x.Mask, x.Left, rr) :> _
            else
                match update None with
                | None -> x :> _
                | Some value ->
                    cnt := !cnt + 1
                    Node.Join(x.Prefix, x, hash, NoCollisionLeaf(hash, key, value))

  
        new(p : uint32, m : uint32, l : AbstractNode<'K, 'V>, r : AbstractNode<'K, 'V>) = 
            { inherit AbstractNode<'K, 'V>(); Prefix = p; Mask = m; Left = l; Right = r }
        


[<Struct>]
type HashMapOkasakiVirtual<'K, 'V> internal(cmp : IEqualityComparer<'K>, root : AbstractNode<'K, 'V>, cnt : int) =

    static member Empty = HashMapOkasakiVirtual<'K, 'V>(EqualityComparer<'K>.Default, Empty.Instance, 0)

    member x.Count = cnt
    member internal x.Root = root
    
    static member OfSeq(elements : seq<'K * 'V>) =  
        let cmp = EqualityComparer<'K>.Default
        let cnt = ref 0
        let mutable r = HashMapOkasakiVirtualImplementation.Empty<'K, 'V>.Instance 
        for (k, v) in elements do
            let hash = cmp.GetHashCode k |> uint32
            r <- r.AddInPlaceUnsafe(cmp, cnt, hash, k, v)
        HashMapOkasakiVirtual<'K, 'V>(cmp, r, !cnt)

    static member OfList(elements : list<'K * 'V>) =  
        let cmp = EqualityComparer<'K>.Default
        let cnt = ref 0
        let mutable r = HashMapOkasakiVirtualImplementation.Empty<'K, 'V>.Instance 
        for (k, v) in elements do
            let hash = cmp.GetHashCode k |> uint32
            r <- r.AddInPlaceUnsafe(cmp, cnt, hash, k, v)
        HashMapOkasakiVirtual<'K, 'V>(cmp, r, !cnt)
        
    static member OfListUnoptimized(elements : list<'K * 'V>) =  
        let cmp = EqualityComparer<'K>.Default
        let cnt = ref 0
        let mutable r = HashMapOkasakiVirtualImplementation.Empty<'K, 'V>.Instance 
        for (k, v) in elements do
            let hash = cmp.GetHashCode k |> uint32
            r <- r.Add(cmp, cnt, hash, k, v)
        HashMapOkasakiVirtual<'K, 'V>(cmp, r, !cnt)

    static member OfArray(elements : array<'K * 'V>) =  
        let cmp = EqualityComparer<'K>.Default
        let cnt = ref 0
        let mutable r = HashMapOkasakiVirtualImplementation.Empty<'K, 'V>.Instance 
        for (k, v) in elements do
            let hash = cmp.GetHashCode k |> uint32
            r <- r.AddInPlaceUnsafe(cmp, cnt, hash, k, v)
        HashMapOkasakiVirtual<'K, 'V>(cmp, r, !cnt)

    member x.Add(key : 'K, value : 'V) =
        let cnt = ref cnt
        let hash = cmp.GetHashCode key |> uint32
        let newRoot = root.Add(cmp, cnt, hash, key, value)
        HashMapOkasakiVirtual(cmp, newRoot, !cnt)
        
    member x.Remove(key : 'K) =
        let cnt = ref cnt
        let hash = cmp.GetHashCode key |> uint32
        let newRoot = root.Remove(cmp, cnt, hash, key)
        HashMapOkasakiVirtual(cmp, newRoot, !cnt)
         
    member x.TryRemove(key : 'K) =
        let cnt = ref cnt
        let hash = cmp.GetHashCode key |> uint32
        match root.TryRemove(cmp, cnt, hash, key) with
        | ValueSome (struct(value, newRoot)) ->
            Some (value, HashMapOkasakiVirtual(cmp, newRoot, !cnt))
        | ValueNone ->
            None
         
    member x.TryFind(key : 'K) =
        let hash = cmp.GetHashCode key |> uint32
        root.TryFind(cmp, hash, key)

    member x.Alter(key : 'K, update : option<'V> -> option<'V>) =
        let cnt = ref cnt
        let hash = cmp.GetHashCode key |> uint32
        let newRoot = root.Alter(cmp, cnt, hash, key, update)
        HashMapOkasakiVirtual(cmp, newRoot, !cnt)

    interface System.Collections.IEnumerable with 
        member x.GetEnumerator() = new HashMapOkasakiVirtualEnumerator<_,_>(root) :> _
        
    interface System.Collections.Generic.IEnumerable<'K * 'V> with 
        member x.GetEnumerator() = new HashMapOkasakiVirtualEnumerator<_,_>(root) :> _

and internal HashMapOkasakiVirtualEnumerator<'K, 'V>(root : AbstractNode<'K, 'V>) =
    let mutable stack = [root]
    let mutable linked : Linked<'K, 'V> = null
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
                stack <- n.Left :: n.Right :: rest
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
        member x.Current = x.Current :> obj
        member x.Reset() = x.Reset()
        
    interface System.Collections.Generic.IEnumerator<'K * 'V> with
        member x.Dispose() = x.Dispose()
        member x.Current = x.Current


module HashMapOkasakiVirtual =

    [<GeneralizableValue>]
    let empty<'K, 'V> = HashMapOkasakiVirtual<'K, 'V>.Empty

    let inline ofSeq (seq : seq<'K * 'V>) =
        HashMapOkasakiVirtual<'K, 'V>.OfSeq seq

    let inline ofList (list : list<'K * 'V>) = 
        HashMapOkasakiVirtual<'K, 'V>.OfList list

    let inline ofArray (arr : array<'K * 'V>) = 
        HashMapOkasakiVirtual<'K, 'V>.OfArray arr
        
    let inline ofListUnoptimized (list : list<'K * 'V>) = 
        HashMapOkasakiVirtual<'K, 'V>.OfListUnoptimized list

    let inline toSeq (m : HashMapOkasakiVirtual<'K, 'V>) = m :> seq<_>
    let inline toList (m : HashMapOkasakiVirtual<'K, 'V>) = m |> Seq.toList
    let inline toArray (m : HashMapOkasakiVirtual<'K, 'V>) = m |> Seq.toArray

    let inline add (key : 'K) (value : 'V) (map : HashMapOkasakiVirtual<'K, 'V>) =
        map.Add(key, value)

    let inline remove (key : 'K) (map : HashMapOkasakiVirtual<'K, 'V>) =
        map.Remove(key)
        
    let inline tryRemove (key : 'K) (map : HashMapOkasakiVirtual<'K, 'V>) =
        map.TryRemove(key)

    let inline tryFind (key : 'K) (map : HashMapOkasakiVirtual<'K, 'V>) =
        map.TryFind(key)

    let inline alter (key : 'K) (update : option<'V> -> option<'V>) (map : HashMapOkasakiVirtual<'K, 'V>) =
        map.Alter(key, update)
