namespace rec ImmutableHashCollections

open System.Collections
open System.Collections.Generic
open System.Runtime.CompilerServices
#if NETCOREAPP3_0 && USE_INTRINSICS
open System.Runtime.Intrinsics.X86
#endif

[<AutoOpen>]
module internal HashMapOkasakiUtilities =

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

[<AutoOpen>]
module internal HashMapOkasakiImplementation = 
    // ========================================================================================================================
    // HashSetNode implementation
    // ========================================================================================================================

    [<AllowNullLiteral>]
    type HashSetLinked<'T> =
        val mutable public Next: HashSetLinked<'T>
        val mutable public Value: 'T

        new(v) = { Value = v; Next = null }
        new(v, n) = { Value = v; Next = n }

    module HashSetLinked =
    
        let rec addInPlaceUnsafe (cmp: EqualityComparer<'T>) (value : 'T) (n: HashSetLinked<'T>) =
            if isNull n then
                HashSetLinked(value)
            elif cmp.Equals(n.Value, value) then
                n.Value <- value
                n
            else
                n.Next <- addInPlaceUnsafe cmp value n.Next
                n

        let rec add (cmp: EqualityComparer<'T>) (value: 'T) (n: HashSetLinked<'T>) =
            if isNull n then
                HashSetLinked(value)
            elif cmp.Equals(n.Value, value) then
                n
            else
                let next = add cmp value n.Next
                if next == n.Next then n
                else HashSetLinked(n.Value, add cmp value n.Next)
               
        let rec alter (cmp: EqualityComparer<'T>) (value: 'T) (update: bool -> bool) (n: HashSetLinked<'T>) =
            if isNull n then
                if update false then HashSetLinked(value)
                else null
            elif cmp.Equals(n.Value, value) then
                if update true then n
                else n.Next
            else
                let next = alter cmp value update n.Next
                if next == n.Next then n
                else HashSetLinked(n.Value, next)
               
        let rec contains (cmp: EqualityComparer<'T>) (value: 'T) (n: HashSetLinked<'T>) =
            if isNull n then false
            elif cmp.Equals(n.Value, value) then true
            else contains cmp value n.Next

        let destruct (n: HashSetLinked<'T>) =
            if isNull n then ValueNone
            else ValueSome(struct (n.Value, n.Next))
            
        let rec remove (cmp: EqualityComparer<'T>) (value: 'T) (n: HashSetLinked<'T>) =
            if isNull n then
                null
            elif cmp.Equals(n.Value, value) then 
                n.Next
            else
                let rest = remove cmp value n.Next
                if rest == n.Next then n
                else HashSetLinked(n.Value, rest)

        let rec tryRemove (cmp: EqualityComparer<'T>) (value: 'T) (n: HashSetLinked<'T>) =
            if isNull n then
                ValueNone
            elif cmp.Equals(n.Value, value) then 
                ValueSome n.Next
            else
                match tryRemove cmp value n.Next with
                | ValueSome rest ->
                    ValueSome (HashSetLinked(n.Value, rest))
                | ValueNone ->
                    ValueNone

        let rec filter (predicate: 'T -> bool) (n: HashSetLinked<'T>) =
            if isNull n then
                null
            elif predicate n.Value then
                let next = filter predicate n.Next
                if n.Next == next then n
                else HashSetLinked(n.Value, filter predicate n.Next)
            else
                filter predicate n.Next
    
        let rec mapToMap (mapping : 'T -> 'R) (n: HashSetLinked<'T>) =
            if isNull n then
                null
            else
                let v = mapping n.Value
                HashMapLinked(n.Value, v, mapToMap mapping n.Next)
                
        let rec chooseToMap (mapping : 'T -> option<'R>) (n: HashSetLinked<'T>) =
            if isNull n then
                null
            else
                match mapping n.Value with
                | Some v -> HashMapLinked(n.Value, v, chooseToMap mapping n.Next)
                | None -> chooseToMap mapping n.Next
                
        let rec chooseToMapV (mapping : 'T -> voption<'R>) (n: HashSetLinked<'T>) =
            if isNull n then
                null
            else
                match mapping n.Value with
                | ValueSome v -> HashMapLinked(n.Value, v, chooseToMapV mapping n.Next)
                | ValueNone -> chooseToMapV mapping n.Next
                
        let rec chooseToMapV2 (mapping : 'T -> struct(voption<'T1> * voption<'T2>)) (n: HashSetLinked<'T>) =
            if isNull n then
                struct(null, null)
            else
                let struct (l, r) = mapping n.Value
                let struct (ln, rn) = chooseToMapV2 mapping n.Next
                
                let l = match l with | ValueSome l -> HashMapLinked(n.Value, l, ln) | ValueNone -> ln
                let r = match r with | ValueSome r -> HashMapLinked(n.Value, r, rn) | ValueNone -> rn
                struct (l, r)

        let rec exists (predicate: 'T -> bool) (n: HashSetLinked<'T>) =
            if isNull n then 
                false
            elif predicate n.Value then
                true
            else
                exists predicate n.Next
                
        let rec forall (predicate: 'T -> bool) (n: HashSetLinked<'T>) =
            if isNull n then 
                true
            elif not (predicate n.Value) then
                false
            else
                forall predicate n.Next

        let rec copyTo (index: ref<int>) (dst : 'T array) (n: HashSetLinked<'T>) =
            if not (isNull n) then
                dst.[!index] <- n.Value
                index := !index + 1
                copyTo index dst n.Next

    [<AbstractClass>]
    type HashSetNode<'T>() =
        abstract member Remove: EqualityComparer<'T> * uint32 * 'T -> HashSetNode<'T>
        abstract member TryRemove: EqualityComparer<'T> * uint32 * 'T -> ValueOption<HashSetNode<'T>>

        abstract member Count : int
        abstract member IsEmpty: bool

        abstract member AddInPlaceUnsafe: EqualityComparer<'T> * uint32 * 'T -> HashSetNode<'T>
        abstract member Add: EqualityComparer<'T> * uint32 * 'T -> HashSetNode<'T>
        abstract member Alter: EqualityComparer<'T> * uint32 * 'T * (bool -> bool) -> HashSetNode<'T>
        abstract member Contains: EqualityComparer<'T> * uint32 * 'T -> bool

        abstract member MapToMap: mapping: ('T -> 'R) -> HashMapNode<'T, 'R>
        abstract member ChooseToMap: mapping: ('T -> option<'R>) -> HashMapNode<'T, 'R>
        abstract member ChooseToMapV: mapping: ('T -> voption<'R>) -> HashMapNode<'T, 'R>
        abstract member ChooseToMapV2: mapping: ('T -> struct(ValueOption<'T1> * ValueOption<'T2>)) -> struct (HashMapNode<'T, 'T1> * HashMapNode<'T, 'T2>)
        abstract member Filter: predicate: ('T -> bool) -> HashSetNode<'T>
        abstract member Iter: action: ('T -> unit) -> unit
        abstract member Fold: acc: OptimizedClosures.FSharpFunc<'S, 'T, 'S> * seed : 'S -> 'S
        abstract member Exists: predicate: ('T -> bool) -> bool
        abstract member Forall: predicate: ('T -> bool) -> bool

        abstract member Accept: HashSetVisitor<'T, 'R> -> 'R

        abstract member ToArray: ref<array<'T>> * ref<int> -> unit
        abstract member CopyTo: dst: 'T array * index : ref<int> -> unit
        
    [<AbstractClass>]
    type HashSetLeaf<'T>() =
        inherit HashSetNode<'T>()
        abstract member LHash : uint32
        abstract member LValue : 'T
        abstract member LNext : HashSetLinked<'T>
        
        static member inline New(h: uint32, v: 'T, n: HashSetLinked<'T>) : HashSetNode<'T> = 
            if isNull n then new HashSetNoCollisionLeaf<_>(Hash = h, Value = v) :> HashSetNode<'T>
            else new HashSetCollisionLeaf<_>(Hash = h, Value = v, Next = n) :> HashSetNode<'T>
  
    [<Sealed>]
    type HashSetEmpty<'T> private() =
        inherit HashSetNode<'T>()
        static let instance = HashSetEmpty<'T>() :> HashSetNode<_>
        static member Instance = instance

        override x.Count = 0

        override x.ToArray(dst, o) =
            ()

        override x.Accept(v: HashSetVisitor<_,_>) =
            v.VisitEmpty x

        override x.IsEmpty = true

        override x.Contains(_cmp: EqualityComparer<'T>, _hash: uint32, _value: 'T) =
            false

        override x.Remove(_cmp: EqualityComparer<'T>, _hash: uint32, _value: 'T) =
            x:> _
            
        override x.TryRemove(_cmp: EqualityComparer<'T>, _hash: uint32, _value: 'T) =
            ValueNone

        override x.AddInPlaceUnsafe(_cmp: EqualityComparer<'T>, hash: uint32, value: 'T) =
            HashSetNoCollisionLeaf.New(hash, value)

        override x.Add(_cmp: EqualityComparer<'T>, hash: uint32, value: 'T) =
            HashSetNoCollisionLeaf.New(hash, value)

        override x.Alter(cmp: EqualityComparer<'T>, hash: uint32, value: 'T, update: bool -> bool) =
            if update false then
                HashSetNoCollisionLeaf.New(hash, value)
            else
                x :> _

        override x.MapToMap(_mapping: 'T -> 'R) =
            HashMapEmpty.Instance
            
        override x.ChooseToMap(_mapping: 'T -> option<'R>) =
            HashMapEmpty.Instance
            
        override x.ChooseToMapV(_mapping: 'T -> ValueOption<'R>) =
            HashMapEmpty.Instance
                 
        override x.ChooseToMapV2(_mapping : 'T -> struct (voption<'T1> * voption<'T2>)) =
            struct(HashMapEmpty.Instance, HashMapEmpty.Instance)
                          
        override x.Filter(_predicate: 'T -> bool) =
            HashSetEmpty.Instance

        override x.Iter(_action: 'T -> unit) =
            ()
            
        override x.Fold(_acc: OptimizedClosures.FSharpFunc<'S, 'T, 'S>, seed : 'S) =
            seed

        override x.Exists(_predicate: 'T -> bool) =
            false

        override x.Forall(_predicate: 'T -> bool) =
            true

        override x.CopyTo(_dst : 'T array, _index : ref<int>) =
            ()
     
    [<Sealed>]
    type HashSetNoCollisionLeaf<'T>() =
        inherit HashSetLeaf<'T>()
        [<DefaultValue>]
        val mutable public Value: 'T
        [<DefaultValue>]
        val mutable public Hash: uint32

        override x.Count = 1
        override x.LHash = x.Hash
        override x.LValue = x.Value
        override x.LNext = null

        override x.ToArray(dst, o) =
            if !o >= dst.Value.Length then System.Array.Resize(&dst.contents, !o * 2)
            dst.Value.[!o] <- x.Value
            o := !o + 1
        
        override x.IsEmpty = false
        
        override x.Accept(v: HashSetVisitor<_,_>) =
            v.VisitNoCollision x

        override x.Contains(cmp: EqualityComparer<'T>, hash: uint32, value: 'T) =   
            if hash = x.Hash && cmp.Equals(value, x.Value) then 
                true
            else
                false

        override x.Remove(cmp: EqualityComparer<'T>, hash: uint32, value: 'T) =
            if hash = x.Hash && cmp.Equals(value, x.Value) then
                HashSetEmpty.Instance
            else
                x:> _

        override x.TryRemove(cmp: EqualityComparer<'T>, hash: uint32, value: 'T) =
            if hash = x.Hash && cmp.Equals(value, x.Value) then
                ValueSome (HashSetEmpty.Instance)
            else
                ValueNone

        override x.AddInPlaceUnsafe(cmp: EqualityComparer<'T>, hash: uint32, value: 'T) =
            if x.Hash = hash then
                if cmp.Equals(value, x.Value) then
                    x.Value <- value
                    x:> _
                else
                    HashSetCollisionLeaf.New(x.Hash, x.Value, HashSetLinked(value, null))
            else
                let n = HashSetNoCollisionLeaf.New(hash, value)
                HashSetInner.Join(hash, n, x.Hash, x)

        override x.Add(cmp: EqualityComparer<'T>, hash: uint32,value: 'T) =
            if x.Hash = hash then
                if cmp.Equals(value, x.Value) then
                    x :> _
                else
                    HashSetCollisionLeaf.New(x.Hash, x.Value, HashSetLinked.add cmp value null)
            else
                let n = HashSetNoCollisionLeaf.New(hash, value)
                HashSetInner.Join(hash, n, x.Hash, x)
        
        override x.Alter(cmp: EqualityComparer<'T>, hash: uint32, value: 'T, update: bool -> bool) =
            if x.Hash = hash then
                if cmp.Equals(value, x.Value) then
                    if update true then
                        x :> _
                    else
                        HashSetEmpty.Instance
                else
                    if update false then
                        HashSetCollisionLeaf.New(x.Hash, x.Value, HashSetLinked(value, null))
                    else
                        x :> _
            else
                if update false then
                    let n = HashSetNoCollisionLeaf.New(hash, value)
                    HashSetInner.Join(hash, n, x.Hash, x)
                else
                    x:> _
           
        override x.MapToMap(mapping: 'T -> 'R) =
            let t = mapping x.Value
            HashMapNoCollisionLeaf.New(x.Hash, x.Value, t)
               
        override x.ChooseToMap(mapping: 'T -> option<'R>) =
            match mapping x.Value with
            | Some v ->
                HashMapNoCollisionLeaf<'T, 'R>.New(x.Hash, x.Value, v)
            | None ->
                HashMapEmpty<'T, 'R>.Instance
                
        override x.ChooseToMapV(mapping: 'T -> voption<'R>) =
            match mapping x.Value with
            | ValueSome v ->
                HashMapNoCollisionLeaf.New(x.Hash, x.Value, v)
            | ValueNone ->
                HashMapEmpty.Instance
 
        override x.ChooseToMapV2(mapping : 'T -> struct (ValueOption<'T1> * ValueOption<'T2>)) =
            let struct (l,r) = mapping x.Value 
            let l = match l with | ValueSome v -> HashMapNoCollisionLeaf.New(x.Hash, x.Value, v) :> HashMapNode<_,_> | _ -> HashMapEmpty.Instance
            let r = match r with | ValueSome v -> HashMapNoCollisionLeaf.New(x.Hash, x.Value, v) :> HashMapNode<_,_> | _ -> HashMapEmpty.Instance
            struct (l, r)

        override x.Filter(predicate: 'T -> bool) =
            if predicate x.Value then x :> _
            else HashSetEmpty.Instance
 
        override x.Iter(action: 'T -> unit) =
            action x.Value
            
        override x.Fold(acc: OptimizedClosures.FSharpFunc<'S, 'T, 'S>, seed : 'S) =
            acc.Invoke(seed, x.Value)

        override x.Exists(predicate: 'T -> bool) =
            predicate x.Value
                
        override x.Forall(predicate: 'T -> bool) =
            predicate x.Value

        override x.CopyTo(dst : 'T array, index : ref<int>) =
            dst.[!index] <- x.Value
            index := !index + 1

        static member New(h : uint32, v : 'T) : HashSetNode<'T> =
            new HashSetNoCollisionLeaf<_>(Hash = h, Value = v) :> HashSetNode<'T>

    [<Sealed>]
    type HashSetCollisionLeaf<'T>() =
        inherit HashSetLeaf<'T>()

        [<DefaultValue>]
        val mutable public Next: HashSetLinked<'T>
        [<DefaultValue>]
        val mutable public Value: 'T
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
        override x.LValue = x.Value
        override x.LNext = x.Next

        override x.ToArray(dst, o) =
            if !o >= dst.Value.Length then System.Array.Resize(&dst.contents, !o * 2)
            dst.Value.[!o] <- x.Value
            o := !o + 1
            
            let mutable n = x.Next
            while not (isNull n) do
                if !o >= dst.Value.Length then System.Array.Resize(&dst.contents, !o * 2)
                dst.Value.[!o] <- n.Value
                o := !o + 1
                n <- n.Next

        override x.Accept(v: HashSetVisitor<_,_>) =
            v.VisitLeaf x

        override x.IsEmpty = false
        
        override x.Contains(cmp: EqualityComparer<'T>, hash: uint32, value: 'T) =   
            if hash = x.Hash then
                if cmp.Equals(value, x.Value) then 
                    true
                else
                    HashSetLinked.contains cmp value x.Next
            else
                false

        override x.Remove(cmp: EqualityComparer<'T>, hash: uint32, value: 'T) =
            if hash = x.Hash then
                if cmp.Equals(value, x.Value) then
                    match HashSetLinked.destruct x.Next with
                    | ValueSome (struct (v, rest)) ->
                        HashSetLeaf.New(hash, v, rest)
                    | ValueNone ->
                        HashSetEmpty.Instance
                else
                    let next = HashSetLinked.remove cmp value x.Next
                    if next == x.Next then x :> _
                    else HashSetLeaf.New(x.Hash, x.Value, next)
            else
                x:> _

        override x.TryRemove(cmp: EqualityComparer<'T>, hash: uint32, value: 'T) =
            if hash = x.Hash then
                if cmp.Equals(value, x.Value) then
                    match HashSetLinked.destruct x.Next with
                    | ValueSome (struct (v, rest)) ->
                        ValueSome (HashSetLeaf.New(hash, v, rest))
                    | ValueNone ->
                        ValueSome  HashSetEmpty.Instance
                else
                    match HashSetLinked.tryRemove cmp value x.Next with
                    | ValueSome rest ->
                        ValueSome(HashSetLeaf.New(x.Hash, x.Value, rest))
                    | ValueNone ->
                        ValueNone
            else
                ValueNone

        override x.AddInPlaceUnsafe(cmp: EqualityComparer<'T>, hash: uint32, value: 'T) =
            if x.Hash = hash then
                if cmp.Equals(value, x.Value) then
                    x.Value <- value
                    x:> _
                else
                    x.Next <- HashSetLinked.addInPlaceUnsafe cmp value x.Next
                    x:> _
            else
                let n = HashSetNoCollisionLeaf.New(hash, value)
                HashSetInner.Join(hash, n, x.Hash, x)
                
        override x.Add(cmp: EqualityComparer<'T>, hash: uint32, value: 'T) =
            if x.Hash = hash then
                if cmp.Equals(value, x.Value) then
                    x :> _
                else
                    HashSetCollisionLeaf.New(x.Hash, x.Value, HashSetLinked.add cmp value x.Next)
            else
                let n = HashSetNoCollisionLeaf.New(hash, value)
                HashSetInner.Join(hash, n, x.Hash, x)

        override x.Alter(cmp: EqualityComparer<'T>, hash: uint32, value: 'T, update: bool -> bool) =
            if x.Hash = hash then
                if cmp.Equals(value, x.Value) then
                    if update true then
                        // update
                        x :> _
                    else
                        // remove
                        match HashSetLinked.destruct x.Next with
                        | ValueSome (struct (v, rest)) ->
                            HashSetLeaf.New(x.Hash, v, rest)
                        | ValueNone ->
                            HashSetEmpty.Instance
                else
                    // in linked?
                    let n = HashSetLinked.alter cmp value update x.Next
                    if n == x.Next then x:> _
                    else HashSetLeaf.New(x.Hash, x.Value, n)
            else
                // other hash => not contained
                if update false then
                    // add
                    let n = HashSetNoCollisionLeaf.New(hash, value)
                    HashSetInner.Join(hash, n, x.Hash, x)
                else 
                    x:> _

        override x.MapToMap(mapping: 'T -> 'R) =
            let t = mapping x.Value
            HashMapCollisionLeaf.New(x.Hash, x.Value, t, HashSetLinked.mapToMap mapping x.Next)
            
        override x.ChooseToMap(mapping: 'T -> option<'R>) =
            match mapping x.Value with
            | Some v ->
                HashMapLeaf.New(x.Hash, x.Value, v, HashSetLinked.chooseToMap mapping x.Next)
            | None -> 
                let rest = HashSetLinked.chooseToMap mapping x.Next
                match HashMapLinked.destruct rest with
                | ValueSome (struct (key, value, rest)) ->
                    HashMapLeaf.New(x.Hash, key, value, rest)
                | ValueNone ->
                    HashMapEmpty.Instance

        override x.ChooseToMapV(mapping: 'T -> voption<'R>) =
            match mapping x.Value with
            | ValueSome v ->
                HashMapLeaf.New(x.Hash, x.Value, v, HashSetLinked.chooseToMapV mapping x.Next)
            | ValueNone -> 
                let rest = HashSetLinked.chooseToMapV mapping x.Next
                match HashMapLinked.destruct rest with
                | ValueSome (struct (key, value, rest)) ->
                    HashMapLeaf.New(x.Hash, key, value, rest)
                | ValueNone ->
                    HashMapEmpty.Instance

        override x.ChooseToMapV2(mapping: 'T -> struct (ValueOption<'T1> * ValueOption<'T2>)) =
            let struct (l,r) = mapping x.Value
            let struct (ln, rn) = HashSetLinked.chooseToMapV2 mapping x.Next
            let left = 
                match l with
                | ValueSome v -> HashMapLeaf.New(x.Hash, x.Value, v, ln) :> HashMapNode<_,_>
                | ValueNone -> 
                    match HashMapLinked.destruct ln with
                    | ValueSome (struct (key, value, rest)) ->
                        HashMapLeaf.New(x.Hash, key, value, rest)
                    | ValueNone ->
                        HashMapEmpty.Instance
            let right = 
                match r with
                | ValueSome v -> HashMapLeaf.New(x.Hash, x.Value, v, rn) :> HashMapNode<_,_>
                | ValueNone -> 
                    match HashMapLinked.destruct rn with
                    | ValueSome (struct (key, value, rest)) ->
                        HashMapLeaf.New(x.Hash, key, value, rest)
                    | ValueNone ->
                        HashMapEmpty.Instance
            struct (left, right)

        override x.Filter(predicate: 'T -> bool) =
            if predicate x.Value then
                HashSetLeaf.New(x.Hash, x.Value, HashSetLinked.filter predicate x.Next)
            else
                let rest = HashSetLinked.filter predicate x.Next
                match HashSetLinked.destruct rest with
                | ValueSome (struct (value, rest)) ->
                    HashSetLeaf.New(x.Hash, value, rest)
                | ValueNone ->
                    HashSetEmpty.Instance

        override x.Iter(action: 'T -> unit) =
            action x.Value
            let mutable n = x.Next
            while not (isNull n) do
                action n.Value
                n <- n.Next
                
        override x.Fold(acc: OptimizedClosures.FSharpFunc<'S, 'T, 'S>, seed : 'S) =
            let mutable res = acc.Invoke(seed, x.Value)
            let mutable n = x.Next
            while not (isNull n) do
                res <- acc.Invoke(res, n.Value)
                n <- n.Next
            res

        override x.Exists(predicate: 'T -> bool) =
            if predicate x.Value then true
            else HashSetLinked.exists predicate x.Next
                
        override x.Forall(predicate: 'T -> bool) =
            if predicate x.Value then HashSetLinked.forall predicate x.Next
            else false

        override x.CopyTo(dst : 'T array, index : ref<int>) =
            dst.[!index] <- x.Value
            index := !index + 1
            HashSetLinked.copyTo index dst x.Next
            
        static member New(h: uint32, v: 'T, n: HashSetLinked<'T>) : HashSetNode<'T> = 
            assert (not (isNull n))
            new HashSetCollisionLeaf<_>(Hash = h, Value = v, Next = n) :> HashSetNode<'T>
 
    [<Sealed>]
    type HashSetInner<'T>() =
        inherit HashSetNode<'T>()
        [<DefaultValue>]
        val mutable public Prefix: uint32
        [<DefaultValue>]
        val mutable public Mask: Mask
        [<DefaultValue>]
        val mutable public Left: HashSetNode<'T>
        [<DefaultValue>]
        val mutable public Right: HashSetNode<'T>
        [<DefaultValue>]
        val mutable public _Count: int

        override x.Count = x._Count

        static member Join (p0 : uint32, t0 : HashSetNode<'T>, p1 : uint32, t1 : HashSetNode<'T>) : HashSetNode<'T>=
            if t0.IsEmpty then t1
            elif t1.IsEmpty then t0
            else
                let m = getMask p0 p1
                if zeroBit p0 m = 0u then HashSetInner.New(getPrefix p0 m, m, t0, t1)
                else HashSetInner.New(getPrefix p0 m, m, t1, t0)

        static member Create(p: uint32, m: Mask, l: HashSetNode<'T>, r: HashSetNode<'T>) =
            if r.IsEmpty then l
            elif l.IsEmpty then r
            else HashSetInner.New(p, m, l, r)

        override x.ToArray(dst, o) =
            x.Left.ToArray(dst, o)
            x.Right.ToArray(dst, o)

        override x.IsEmpty = false
        
        override x.Accept(v: HashSetVisitor<_,_>) =
            v.VisitNode x

        override x.Contains(cmp: EqualityComparer<'T>, hash: uint32, value: 'T) =
            #if OPTIMISTIC 
            let m = zeroBit hash x.Mask
            if m = 0u then x.Left.Contains(cmp, hash, value)
            else x.Right.Contains(cmp, hash, value)
            #else
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then x.Left.Contains(cmp, hash, value)
            elif m = 1u then x.Right.Contains(cmp, hash, value)
            else false
            #endif

        override x.Remove(cmp: EqualityComparer<'T>, hash: uint32, value: 'T) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                let l = x.Left.Remove(cmp, hash, value)
                if l == x.Left then x :> _
                else HashSetInner.Create(x.Prefix, x.Mask, l, x.Right)
            elif m = 1u then
                let r = x.Right.Remove(cmp, hash, value)
                if r == x.Right then x :> _
                else HashSetInner.Create(x.Prefix, x.Mask, x.Left, r)
            else
                x:> _

        override x.TryRemove(cmp: EqualityComparer<'T>, hash: uint32, value: 'T) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                match x.Left.TryRemove(cmp, hash, value) with
                | ValueSome ll ->
                    ValueSome (HashSetInner.Create(x.Prefix, x.Mask, ll, x.Right))
                | ValueNone ->
                    ValueNone
            elif m = 1u then
                match x.Right.TryRemove(cmp, hash, value) with
                | ValueSome rr ->
                    ValueSome (HashSetInner.Create(x.Prefix, x.Mask, x.Left, rr))
                | ValueNone ->
                    ValueNone
            else
                ValueNone

        override x.AddInPlaceUnsafe(cmp: EqualityComparer<'T>, hash: uint32, value: 'T) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                x.Left <- x.Left.AddInPlaceUnsafe(cmp, hash, value)
                x._Count <- x.Left.Count + x.Right.Count
                x:> HashSetNode<_>
            elif m = 1u then 
                x.Right <- x.Right.AddInPlaceUnsafe(cmp, hash, value)
                x._Count <- x.Left.Count + x.Right.Count
                x:> HashSetNode<_>
            else
                let n = HashSetNoCollisionLeaf.New(hash, value)
                HashSetInner.Join(x.Prefix, x, hash, n)

        override x.Add(cmp: EqualityComparer<'T>, hash: uint32, value: 'T) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                HashSetInner.New(x.Prefix, x.Mask, x.Left.Add(cmp, hash, value), x.Right)
            elif m = 1u then 
                HashSetInner.New(x.Prefix, x.Mask, x.Left, x.Right.Add(cmp, hash, value))
            else
                let n = HashSetNoCollisionLeaf.New(hash, value)
                HashSetInner.Join(x.Prefix, x, hash, n)

        override x.Alter(cmp: EqualityComparer<'T>, hash: uint32, value: 'T, update: bool -> bool) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                let ll = x.Left.Alter(cmp, hash, value, update)
                if ll == x.Left then x:> _
                else HashSetInner.New(x.Prefix, x.Mask, ll, x.Right)
            elif m = 1u then
                let rr = x.Right.Alter(cmp, hash, value, update)
                if rr == x.Right then x:> _
                else HashSetInner.New(x.Prefix, x.Mask, x.Left, rr)
            else
                if update false then
                    let n = HashSetNoCollisionLeaf.New(hash, value)
                    HashSetInner.Join(x.Prefix, x, hash, n)
                else
                    x:> _
                    
        override x.MapToMap(mapping: 'T -> 'R) =
            HashMapInner.New(x.Prefix, x.Mask, x.Left.MapToMap(mapping), x.Right.MapToMap(mapping))
  
        override x.ChooseToMap(mapping: 'T -> option<'R>) =
            HashMapInner.Create(x.Prefix, x.Mask, x.Left.ChooseToMap(mapping), x.Right.ChooseToMap(mapping))
            
        override x.ChooseToMapV(mapping: 'T -> voption<'R>) =
            HashMapInner.Create(x.Prefix, x.Mask, x.Left.ChooseToMapV(mapping), x.Right.ChooseToMapV(mapping))
      
        override x.ChooseToMapV2(mapping: 'T -> struct(ValueOption<'T1> * ValueOption<'T2>)) =
            let struct (la, lb) = x.Left.ChooseToMapV2(mapping)
            let struct (ra, rb) = x.Right.ChooseToMapV2(mapping)

            struct (
                HashMapInner.Create(x.Prefix, x.Mask, la, ra),
                HashMapInner.Create(x.Prefix, x.Mask, lb, rb)
            )
      
        override x.Filter(predicate: 'T -> bool) =
            HashSetInner.Create(x.Prefix, x.Mask, x.Left.Filter(predicate), x.Right.Filter(predicate))
            
        override x.Iter(action: 'T -> unit) =
            x.Left.Iter(action)
            x.Right.Iter(action)

        override x.Fold(acc: OptimizedClosures.FSharpFunc<'S, 'T, 'S>, seed : 'S) =
            let s = x.Left.Fold(acc, seed)
            x.Right.Fold(acc, s)
            

        override x.Exists(predicate: 'T -> bool) =
            x.Left.Exists predicate || x.Right.Exists predicate
                
        override x.Forall(predicate: 'T -> bool) =
            x.Left.Forall predicate && x.Right.Forall predicate

        override x.CopyTo(dst : 'T array, index : ref<int>) =
            x.Left.CopyTo(dst, index)
            x.Right.CopyTo(dst, index)

        static member New(p: uint32, m: Mask, l: HashSetNode<'T>, r: HashSetNode<'T>) : HashSetNode<'T> = 
            new HashSetInner<_>(Prefix = p, Mask = m, Left = l, Right = r, _Count = l.Count + r.Count) :> _

    // ========================================================================================================================
    // HashMapNode implementation
    // ========================================================================================================================

    [<AllowNullLiteral>]
    type HashMapLinked<'K, 'V> =
        val mutable public Next: HashMapLinked<'K, 'V>
        val mutable public Key: 'K
        val mutable public Value: 'V

        new(k : 'K, v : 'V) = { Key = k; Value = v; Next = null }
        new(k : 'K, v : 'V, n : HashMapLinked<'K, 'V>) = { Key = k; Value = v; Next = n }

    module HashMapLinked =
    
        let rec keys (n: HashMapLinked<'K, 'V>) =
            if isNull n then
                null
            else
                HashSetLinked<'K>(n.Key, keys n.Next)
                

        let rec addInPlaceUnsafe (cmp: EqualityComparer<'K>) (key: 'K) (value: 'V) (n: HashMapLinked<'K, 'V>) =
            if isNull n then
                HashMapLinked(key, value)
            elif cmp.Equals(n.Key, key) then
                n.Key <- key
                n.Value <- value
                n
            else
                n.Next <- addInPlaceUnsafe cmp key value n.Next
                n

        let rec add (cmp: EqualityComparer<'K>) (key: 'K) (value: 'V) (n: HashMapLinked<'K, 'V>) =
            if isNull n then
                HashMapLinked(key, value)
            elif cmp.Equals(n.Key, key) then
                HashMapLinked(key, value, n.Next)
            else
                HashMapLinked(n.Key, n.Value, add cmp key value n.Next)
               
        let rec alter (cmp: EqualityComparer<'K>) (key: 'K) (update: option<'V> -> option<'V>) (n: HashMapLinked<'K, 'V>) =
            if isNull n then
                match update None with
                | Some value -> 
                    HashMapLinked(key, value)
                | None ->
                    null
            elif cmp.Equals(n.Key, key) then
                match update (Some n.Value) with
                | Some value -> 
                    HashMapLinked(key, value, n.Next)
                | None -> 
                    n.Next
            else
                let next = alter cmp key update n.Next
                if next == n.Next then n
                else HashMapLinked(n.Key, n.Value, next)
               
        let rec alterV (cmp: EqualityComparer<'K>) (key: 'K) (update: voption<'V> -> voption<'V>) (n: HashMapLinked<'K, 'V>) =
            if isNull n then
                match update ValueNone with
                | ValueSome value -> 
                    HashMapLinked(key, value)
                | ValueNone ->
                    null
            elif cmp.Equals(n.Key, key) then
                match update (ValueSome n.Value) with
                | ValueSome value -> 
                    HashMapLinked(key, value, n.Next)
                | ValueNone -> 
                    n.Next
            else
                let next = alterV cmp key update n.Next
                if next == n.Next then n
                else HashMapLinked(n.Key, n.Value, next)
               
        let rec tryFind (cmp: EqualityComparer<'K>) (key: 'K) (n: HashMapLinked<'K, 'V>) =
            if isNull n then None
            elif cmp.Equals(n.Key, key) then Some n.Value
            else tryFind cmp key n.Next
            
        let rec tryFindV (cmp: EqualityComparer<'K>) (key: 'K) (n: HashMapLinked<'K, 'V>) =
            if isNull n then ValueNone
            elif cmp.Equals(n.Key, key) then ValueSome n.Value
            else tryFindV cmp key n.Next
            
        let rec containsKey (cmp: EqualityComparer<'K>) (key: 'K) (n: HashMapLinked<'K, 'V>) =
            if isNull n then false
            elif cmp.Equals(n.Key, key) then true
            else containsKey cmp key n.Next

        let destruct<'K, 'V> (n: HashMapLinked<'K, 'V>) : voption<struct('K * 'V * HashMapLinked<'K, 'V>)> =
            if isNull n then ValueNone
            else ValueSome(struct (n.Key, n.Value, n.Next))
            
        let rec remove (cmp: EqualityComparer<'K>) (key: 'K) (n: HashMapLinked<'K, 'V>) =
            if isNull n then
                null
            elif cmp.Equals(n.Key, key) then 
                n.Next
            else
                let rest = remove cmp key n.Next
                if rest == n.Next then n
                else HashMapLinked(n.Key, n.Value, rest)

        let rec tryRemove (cmp: EqualityComparer<'K>) (key: 'K) (n: HashMapLinked<'K, 'V>) =
            if isNull n then
                ValueNone
            elif cmp.Equals(n.Key, key) then 
                ValueSome (struct(n.Value, n.Next))
            else
                match tryRemove cmp key n.Next with
                | ValueSome (struct (value, rest)) ->
                    ValueSome(struct(value, HashMapLinked(n.Key, n.Value, rest)))
                | ValueNone ->
                    ValueNone

        let rec map (mapping: OptimizedClosures.FSharpFunc<'K, 'V, 'T>) (n: HashMapLinked<'K, 'V>) = 
            if isNull n then
                null
            else 
                let r = mapping.Invoke(n.Key, n.Value)
                HashMapLinked(n.Key, r, map mapping n.Next)

        let rec choose (mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>) (n: HashMapLinked<'K, 'V>) = 
            if isNull n then
                null
            else 
                match mapping.Invoke(n.Key, n.Value) with
                | Some r -> 
                    HashMapLinked(n.Key, r, choose mapping n.Next)
                | None -> 
                    choose mapping n.Next
    
        let rec chooseV (mapping: OptimizedClosures.FSharpFunc<'K, 'V, ValueOption<'T>>) (n: HashMapLinked<'K, 'V>) = 
            if isNull n then
                null
            else 
                match mapping.Invoke(n.Key, n.Value) with
                | ValueSome r -> 
                    HashMapLinked(n.Key, r, chooseV mapping n.Next)
                | ValueNone -> 
                    chooseV mapping n.Next
    
        let rec chooseV2 (mapping: OptimizedClosures.FSharpFunc<'K, 'V, struct(ValueOption<'T1> * ValueOption<'T2>)>) (n: HashMapLinked<'K, 'V>) = 
            if isNull n then
                struct(null, null)
            else 
                let struct (l, r) = mapping.Invoke(n.Key, n.Value)
                let struct (lr, rr) = chooseV2 mapping n.Next

                let left = 
                    match l with
                    | ValueSome l -> HashMapLinked(n.Key, l, lr)
                    | ValueNone -> lr
                let right =
                    match r with
                    | ValueSome r -> HashMapLinked(n.Key, r, rr)
                    | ValueNone -> rr
                struct(left, right)

        let rec chooseSV2 (mapping: OptimizedClosures.FSharpFunc<'K, 'V, struct(bool * ValueOption<'T2>)>) (n: HashMapLinked<'K, 'V>) = 
            if isNull n then
                struct(null, null)
            else 
                let struct (l, r) = mapping.Invoke(n.Key, n.Value)
                let struct (lr, rr) = chooseSV2 mapping n.Next

                let left = 
                    if l then HashSetLinked(n.Key, lr)
                    else lr

                let right =
                    match r with
                    | ValueSome r -> HashMapLinked(n.Key, r, rr)
                    | ValueNone -> rr
                struct(left, right)
    
    
        let rec filter (predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) (n: HashMapLinked<'K, 'V>) =
            if isNull n then
                null
            elif predicate.Invoke(n.Key, n.Value) then
                HashMapLinked(n.Key, n.Value, filter predicate n.Next)
            else
                filter predicate n.Next
    
        let rec exists (predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) (n: HashMapLinked<'K, 'V>) =
            if isNull n then 
                false
            elif predicate.Invoke(n.Key, n.Value) then
                true
            else
                exists predicate n.Next
                
        let rec forall (predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) (n: HashMapLinked<'K, 'V>) =
            if isNull n then 
                true
            elif not (predicate.Invoke(n.Key, n.Value)) then
                false
            else
                forall predicate n.Next

        let rec copyTo (index: ref<int>) (dst : ('K * 'V) array) (n: HashMapLinked<'K, 'V>) =
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
        abstract member ChooseSV2: mapping: OptimizedClosures.FSharpFunc<'K, 'V, struct(bool * ValueOption<'T2>)> -> struct (HashSetNode<'K> * HashMapNode<'K, 'T2>)

        abstract member Filter: mapping: OptimizedClosures.FSharpFunc<'K, 'V, bool> -> HashMapNode<'K, 'V>
        abstract member Iter: action: OptimizedClosures.FSharpFunc<'K, 'V, unit> -> unit
        abstract member Fold: acc: OptimizedClosures.FSharpFunc<'S, 'K, 'V, 'S> * seed : 'S -> 'S
        abstract member Exists: predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool> -> bool
        abstract member Forall: predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool> -> bool

        abstract member GetKeys : unit -> HashSetNode<'K>

        abstract member Accept: HashMapVisitor<'K, 'V, 'R> -> 'R

        abstract member ToArray: ref<array<struct('K * 'V)>> * ref<int> -> unit

        abstract member CopyTo: dst: ('K * 'V) array * index : ref<int> -> unit

    [<AbstractClass>]
    type HashMapLeaf<'K, 'V>() =
        inherit HashMapNode<'K, 'V>()
        abstract member LHash : uint32
        abstract member LKey : 'K
        abstract member LValue : 'V
        abstract member LNext : HashMapLinked<'K, 'V>
        
        static member New(h: uint32, k: 'K, v: 'V, n: HashMapLinked<'K, 'V>) : HashMapNode<'K, 'V> = 
            if isNull n then new HashMapNoCollisionLeaf<_,_>(Hash = h, Key = k, Value = v) :> HashMapNode<'K, 'V>
            else new HashMapCollisionLeaf<_,_>(Hash = h, Key = k, Value = v, Next = n) :> HashMapNode<'K, 'V>
     
    [<Sealed>]
    type HashMapEmpty<'K, 'V> private() =
        inherit HashMapNode<'K, 'V>()
        static let instance = HashMapEmpty<'K, 'V>() :> HashMapNode<_,_>
        static member Instance : HashMapNode<'K, 'V> = instance

        override x.Count = 0

        override x.ToArray(dst, o) =
            ()

        override x.GetKeys() =
            HashSetEmpty<'K>.Instance

        override x.Accept(v: HashMapVisitor<_,_,_>) =
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
            HashMapNoCollisionLeaf.New(hash, key, value)

        override x.Add(_cmp: EqualityComparer<'K>, hash: uint32, key: 'K, value: 'V) =
            HashMapNoCollisionLeaf.New(hash, key, value)

        override x.Alter(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, update: option<'V> -> option<'V>) =
            match update None with
            | None -> x:> _
            | Some value ->
                HashMapNoCollisionLeaf.New(hash, key, value)
                
        override x.AlterV(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, update: voption<'V> -> voption<'V>) =
            match update ValueNone with
            | ValueNone -> x:> _
            | ValueSome value ->
                HashMapNoCollisionLeaf.New(hash, key, value)

        override x.Map(_mapping: OptimizedClosures.FSharpFunc<'K, 'V, 'T>) =
            HashMapEmpty.Instance
            
        override x.Choose(_mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>) =
            HashMapEmpty.Instance
            
        override x.ChooseV(_mapping: OptimizedClosures.FSharpFunc<'K, 'V, ValueOption<'T>>) =
            HashMapEmpty.Instance
                 
        override x.ChooseV2(_mapping) =
            struct(HashMapEmpty.Instance, HashMapEmpty.Instance)
                 
        override x.ChooseSV2(_mapping) =
            struct(HashSetEmpty.Instance, HashMapEmpty.Instance)
                                
        override x.Filter(_predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            HashMapEmpty.Instance

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

    [<Sealed>]
    type HashMapCollisionLeaf<'K, 'V>() =
        inherit HashMapLeaf<'K, 'V>()

        [<DefaultValue>]
        val mutable public Next: HashMapLinked<'K, 'V>
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
            

        override x.GetKeys() =
            HashSetCollisionLeaf<'K>.New(x.Hash, x.Key, HashMapLinked.keys x.Next)

        override x.Accept(v: HashMapVisitor<_,_,_>) =
            v.VisitLeaf x

        override x.IsEmpty = false
        
        override x.TryFind(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =   
            if hash = x.Hash then
                if cmp.Equals(key, x.Key) then 
                    Some x.Value
                else
                    HashMapLinked.tryFind cmp key x.Next
            else
                None

        override x.TryFindV(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =   
            if hash = x.Hash then
                if cmp.Equals(key, x.Key) then 
                    ValueSome x.Value
                else
                    HashMapLinked.tryFindV cmp key x.Next
            else
                ValueNone

        override x.ContainsKey(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =   
            if hash = x.Hash then
                if cmp.Equals(key, x.Key) then 
                    true
                else
                    HashMapLinked.containsKey cmp key x.Next
            else
                false

        override x.Remove(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            if hash = x.Hash then
                if cmp.Equals(key, x.Key) then
                    match HashMapLinked.destruct x.Next with
                    | ValueSome (struct (k, v, rest)) ->
                        HashMapLeaf.New(hash, k, v, rest)
                    | ValueNone ->
                        HashMapEmpty.Instance
                else
                    let next = HashMapLinked.remove cmp key x.Next
                    if next == x.Next then x :> _
                    else HashMapLeaf.New(x.Hash, x.Key, x.Value, next)
            else
                x:> _

        override x.TryRemove(cmp: EqualityComparer<'K>, hash: uint32, key: 'K)         =
            if hash = x.Hash then
                if cmp.Equals(key, x.Key) then
                    match HashMapLinked.destruct x.Next with
                    | ValueSome (struct (k, v, rest)) ->
                        ValueSome(struct(x.Value, HashMapLeaf.New(hash, k, v, rest)))
                    | ValueNone ->
                        ValueSome(struct(x.Value, HashMapEmpty.Instance))
                else
                    match HashMapLinked.tryRemove cmp key x.Next with
                    | ValueSome(struct(value, rest)) ->
                        ValueSome(
                            struct(
                                value,
                                HashMapLeaf.New(x.Hash, x.Key, x.Value, rest)
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
                    x.Next <- HashMapLinked.addInPlaceUnsafe cmp key value x.Next
                    x:> _
            else
                let n = HashMapNoCollisionLeaf.New(hash, key, value)
                HashMapInner.Join(hash, n, x.Hash, x)
                
        override x.Add(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, value: 'V) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    HashMapCollisionLeaf.New(x.Hash, key, value, x.Next)
                else
                    HashMapCollisionLeaf.New(x.Hash, x.Key, x.Value, HashMapLinked.add cmp key value x.Next)
            else
                let n = HashMapNoCollisionLeaf.New(hash, key, value)
                HashMapInner.Join(hash, n, x.Hash, x)

        override x.Alter(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, update: option<'V> -> option<'V>) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    match update (Some x.Value) with
                    | None ->
                        // remove
                        match HashMapLinked.destruct x.Next with
                        | ValueSome (struct (k, v, rest)) ->
                            HashMapLeaf.New(x.Hash, k, v, rest)
                        | ValueNone ->
                            HashMapEmpty.Instance
                    | Some value ->
                        // update
                        HashMapCollisionLeaf.New(x.Hash, x.Key, value, x.Next) 
                else
                    // in linked?
                    let n = HashMapLinked.alter cmp key update x.Next
                    if n == x.Next then x:> _
                    else HashMapLeaf.New(x.Hash, x.Key, x.Value, n)
            else
                // other hash => not contained
                match update None with
                | None -> x:> _
                | Some value ->
                    // add
                    let n = HashMapNoCollisionLeaf.New(hash, key, value)
                    HashMapInner.Join(hash, n, x.Hash, x)

        override x.AlterV(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, update: voption<'V> -> voption<'V>) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    match update (ValueSome x.Value) with
                    | ValueNone ->
                        // remove
                        match HashMapLinked.destruct x.Next with
                        | ValueSome (struct (k, v, rest)) ->
                            HashMapLeaf.New(x.Hash, k, v, rest)
                        | ValueNone ->
                            HashMapEmpty.Instance
                    | ValueSome value ->
                        // update
                        HashMapCollisionLeaf.New(x.Hash, x.Key, value, x.Next) 
                else
                    // in linked?
                    let n = HashMapLinked.alterV cmp key update x.Next
                    if n == x.Next then x:> _
                    else HashMapLeaf.New(x.Hash, x.Key, x.Value, n)
            else
                // other hash => not contained
                match update ValueNone with
                | ValueNone -> x:> _
                | ValueSome value ->
                    // add
                    let n = HashMapNoCollisionLeaf.New(hash, key, value)
                    HashMapInner.Join(hash, n, x.Hash, x)

        override x.Map(mapping: OptimizedClosures.FSharpFunc<'K, 'V, 'T>) =
            let t = mapping.Invoke(x.Key, x.Value)
            HashMapCollisionLeaf.New(x.Hash, x.Key, t, HashMapLinked.map mapping x.Next)
            
        override x.Choose(mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>) =
            match mapping.Invoke(x.Key, x.Value) with
            | Some v ->
                HashMapLeaf.New(x.Hash, x.Key, v, HashMapLinked.choose mapping x.Next)
            | None -> 
                let rest = HashMapLinked.choose mapping x.Next
                match HashMapLinked.destruct rest with
                | ValueSome (struct (key, value, rest)) ->
                    HashMapLeaf.New(x.Hash, key, value, rest)
                | ValueNone ->
                    HashMapEmpty.Instance

        override x.ChooseV(mapping: OptimizedClosures.FSharpFunc<'K, 'V, ValueOption<'T>>) =
            match mapping.Invoke(x.Key, x.Value) with
            | ValueSome v ->
                HashMapLeaf.New(x.Hash, x.Key, v, HashMapLinked.chooseV mapping x.Next)
            | ValueNone -> 
                let rest = HashMapLinked.chooseV mapping x.Next
                match HashMapLinked.destruct rest with
                | ValueSome (struct (key, value, rest)) ->
                    HashMapLeaf.New(x.Hash, key, value, rest)
                | ValueNone ->
                    HashMapEmpty.Instance

        override x.ChooseV2(mapping: OptimizedClosures.FSharpFunc<'K, 'V, struct (ValueOption<'T1> * ValueOption<'T2>)>) =
            let struct (l,r) = mapping.Invoke(x.Key, x.Value)
            let struct (ln, rn) = HashMapLinked.chooseV2 mapping x.Next
            let left = 
                match l with
                | ValueSome v -> HashMapLeaf.New(x.Hash, x.Key, v, ln) :> HashMapNode<_,_>
                | ValueNone -> 
                    match HashMapLinked.destruct ln with
                    | ValueSome (struct (key, value, rest)) ->
                        HashMapLeaf.New(x.Hash, key, value, rest)
                    | ValueNone ->
                        HashMapEmpty.Instance
            let right = 
                match r with
                | ValueSome v -> HashMapLeaf.New(x.Hash, x.Key, v, rn) :> HashMapNode<_,_>
                | ValueNone -> 
                    match HashMapLinked.destruct rn with
                    | ValueSome (struct (key, value, rest)) ->
                        HashMapLeaf.New(x.Hash, key, value, rest)
                    | ValueNone ->
                        HashMapEmpty.Instance
            struct (left, right)

        override x.ChooseSV2(mapping: OptimizedClosures.FSharpFunc<'K, 'V, struct (bool * ValueOption<'T2>)>) =
            let struct (l,r) = mapping.Invoke(x.Key, x.Value)
            let struct (ln, rn) = HashMapLinked.chooseSV2 mapping x.Next
            let left = 
                if l then
                    HashSetLeaf.New(x.Hash, x.Key, ln)
                else
                    match HashSetLinked.destruct ln with
                    | ValueSome (struct (value, rest)) ->
                        HashSetLeaf.New(x.Hash, value, rest)
                    | ValueNone ->
                        HashSetEmpty.Instance
            let right = 
                match r with
                | ValueSome v -> HashMapLeaf.New(x.Hash, x.Key, v, rn) :> HashMapNode<_,_>
                | ValueNone -> 
                    match HashMapLinked.destruct rn with
                    | ValueSome (struct (key, value, rest)) ->
                        HashMapLeaf.New(x.Hash, key, value, rest)
                    | ValueNone ->
                        HashMapEmpty.Instance
            struct (left, right)

        override x.Filter(predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            if predicate.Invoke(x.Key, x.Value) then
                HashMapLeaf.New(x.Hash, x.Key, x.Value, HashMapLinked.filter predicate x.Next)
            else
                let rest = HashMapLinked.filter predicate x.Next
                match HashMapLinked.destruct rest with
                | ValueSome (struct (key, value, rest)) ->
                    HashMapLeaf.New(x.Hash, key, value, rest)
                | ValueNone ->
                    HashMapEmpty.Instance

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
            else HashMapLinked.exists predicate x.Next
                
        override x.Forall(predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            if predicate.Invoke(x.Key, x.Value) then HashMapLinked.forall predicate x.Next
            else false

        override x.CopyTo(dst : ('K * 'V) array, index : ref<int>) =
            dst.[!index] <- (x.Key, x.Value)
            index := !index + 1
            HashMapLinked.copyTo index dst x.Next
            
        static member New(h: uint32, k: 'K, v: 'V, n: HashMapLinked<'K, 'V>) : HashMapNode<'K, 'V> = 
            assert (not (isNull n))
            new HashMapCollisionLeaf<_,_>(Hash = h, Key = k, Value = v, Next = n) :> HashMapNode<'K, 'V>
     
    [<Sealed>]
    type HashMapNoCollisionLeaf<'K, 'V>() =
        inherit HashMapLeaf<'K, 'V>()
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

        override x.GetKeys() =
            HashSetNoCollisionLeaf.New(x.Hash, x.Key)

        override x.ToArray(dst, o) =
            if !o >= dst.Value.Length then System.Array.Resize(&dst.contents, !o * 2)
            dst.Value.[!o] <- struct(x.Key, x.Value)
            o := !o + 1
        
        override x.IsEmpty = false
        
        override x.Accept(v: HashMapVisitor<_,_,_>) =
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
                HashMapEmpty.Instance
            else
                x:> _

        override x.TryRemove(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            if hash = x.Hash && cmp.Equals(key, x.Key) then
                ValueSome (struct(x.Value, HashMapEmpty.Instance))
            else
                ValueNone

        override x.AddInPlaceUnsafe(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, value: 'V) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    x.Key <- key
                    x.Value <- value
                    x:> _
                else
                    HashMapCollisionLeaf.New(x.Hash, x.Key, x.Value, HashMapLinked(key, value, null))
            else
                let n = HashMapNoCollisionLeaf.New(hash, key, value)
                HashMapInner.Join(hash, n, x.Hash, x)

        override x.Add(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, value: 'V) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    HashMapNoCollisionLeaf.New(x.Hash, key, value)
                else
                    HashMapCollisionLeaf.New(x.Hash, x.Key, x.Value, HashMapLinked.add cmp key value null)
            else
                let n = HashMapNoCollisionLeaf.New(hash, key, value)
                HashMapInner.Join(hash, n, x.Hash, x)
        
        override x.Alter(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, update: option<'V> -> option<'V>) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    match update (Some x.Value) with
                    | Some value -> 
                        HashMapNoCollisionLeaf.New(x.Hash, x.Key, value)
                    | None -> 
                        HashMapEmpty.Instance
                else
                    match update None with
                    | None -> x:> _
                    | Some value ->
                        HashMapCollisionLeaf.New(x.Hash, x.Key, x.Value, HashMapLinked(key, value, null))
            else
                match update None with
                | None -> x:> _
                | Some value ->
                    let n = HashMapNoCollisionLeaf.New(hash, key, value)
                    HashMapInner.Join(hash, n, x.Hash, x)
           
        override x.AlterV(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, update: voption<'V> -> voption<'V>) =
            if x.Hash = hash then
                if cmp.Equals(key, x.Key) then
                    match update (ValueSome x.Value) with
                    | ValueSome value -> 
                        HashMapNoCollisionLeaf.New(x.Hash, x.Key, value)
                    | ValueNone -> 
                        HashMapEmpty.Instance
                else
                    match update ValueNone with
                    | ValueNone -> x:> _
                    | ValueSome value ->
                        HashMapCollisionLeaf.New(x.Hash, x.Key, x.Value, HashMapLinked(key, value, null))
            else
                match update ValueNone with
                | ValueNone -> x:> _
                | ValueSome value ->
                    let n = HashMapNoCollisionLeaf.New(hash, key, value)
                    HashMapInner.Join(hash, n, x.Hash, x)
           
        override x.Map(mapping: OptimizedClosures.FSharpFunc<'K, 'V, 'T>) =
            let t = mapping.Invoke(x.Key, x.Value)
            HashMapNoCollisionLeaf.New(x.Hash, x.Key, t)
               
        override x.Choose(mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>) =
            match mapping.Invoke(x.Key, x.Value) with
            | Some v ->
                HashMapNoCollisionLeaf.New(x.Hash, x.Key, v)
            | None ->
                HashMapEmpty.Instance
                
        override x.ChooseV(mapping: OptimizedClosures.FSharpFunc<'K, 'V, ValueOption<'T>>) =
            match mapping.Invoke(x.Key, x.Value) with
            | ValueSome v ->
                HashMapNoCollisionLeaf.New(x.Hash, x.Key, v)
            | ValueNone ->
                HashMapEmpty.Instance
 
        override x.ChooseV2(mapping: OptimizedClosures.FSharpFunc<'K, 'V, struct (ValueOption<'T1> * ValueOption<'T2>)>) =
            let struct (l,r) = mapping.Invoke(x.Key, x.Value)         
            let l = match l with | ValueSome v -> HashMapNoCollisionLeaf.New(x.Hash, x.Key, v) :> HashMapNode<_,_> | _ -> HashMapEmpty.Instance
            let r = match r with | ValueSome v -> HashMapNoCollisionLeaf.New(x.Hash, x.Key, v) :> HashMapNode<_,_> | _ -> HashMapEmpty.Instance
            struct (l, r)

        override x.ChooseSV2(mapping: OptimizedClosures.FSharpFunc<'K, 'V, struct (bool * ValueOption<'T2>)>) =
            let struct (l,r) = mapping.Invoke(x.Key, x.Value)         
            let l = if l then HashSetNoCollisionLeaf.New(x.Hash, x.Key) else HashSetEmpty.Instance
            let r = match r with | ValueSome v -> HashMapNoCollisionLeaf.New(x.Hash, x.Key, v) :> HashMapNode<_,_> | _ -> HashMapEmpty.Instance
            struct (l, r)

        override x.Filter(predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            if predicate.Invoke(x.Key, x.Value) then
                HashMapNoCollisionLeaf.New(x.Hash, x.Key, x.Value)
            else
                HashMapEmpty.Instance
 
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
            new HashMapNoCollisionLeaf<_,_>(Hash = h, Key = k, Value = v) :> HashMapNode<'K, 'V>

    [<Sealed>]
    type HashMapInner<'K, 'V>() =
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

        override x.GetKeys() =
            HashSetInner.New(x.Prefix, x.Mask, x.Left.GetKeys(), x.Right.GetKeys())

        override x.Count = x._Count

        static member Join (p0 : uint32, t0 : HashMapNode<'K, 'V>, p1 : uint32, t1 : HashMapNode<'K, 'V>) : HashMapNode<'K,'V>=
            let m = getMask p0 p1
            if zeroBit p0 m = 0u then HashMapInner.New(getPrefix p0 m, m, t0, t1)
            else HashMapInner.New(getPrefix p0 m, m, t1, t0)

        static member Create(p: uint32, m: Mask, l: HashMapNode<'K, 'V>, r: HashMapNode<'K, 'V>) : HashMapNode<'K, 'V> =
            if r.IsEmpty then l
            elif l.IsEmpty then r
            else HashMapInner.New(p, m, l, r)

        override x.ToArray(dst, o) =
            x.Left.ToArray(dst, o)
            x.Right.ToArray(dst, o)

        override x.IsEmpty = false
        
        override x.Accept(v: HashMapVisitor<_,_,_>) =
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
                else HashMapInner.Create(x.Prefix, x.Mask, l, x.Right)
            elif m = 1u then
                let r = x.Right.Remove(cmp, hash, key)
                if r == x.Right then x :> _
                else HashMapInner.Create(x.Prefix, x.Mask, x.Left, r)
            else
                x:> _

        override x.TryRemove(cmp: EqualityComparer<'K>, hash: uint32, key: 'K) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                match x.Left.TryRemove(cmp, hash, key) with
                | ValueSome (struct(value, ll)) ->
                    ValueSome (struct(value, HashMapInner.Create(x.Prefix, x.Mask, ll, x.Right)))
                | ValueNone ->
                    ValueNone
            elif m = 1u then
                match x.Right.TryRemove(cmp, hash, key) with
                | ValueSome (struct(value, rr)) ->
                    ValueSome (struct(value, HashMapInner.Create(x.Prefix, x.Mask, x.Left, rr)))
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
                let n = HashMapNoCollisionLeaf.New(hash, key, value)
                HashMapInner.Join(x.Prefix, x, hash, n)

        override x.Add(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, value: 'V) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                HashMapInner.New(x.Prefix, x.Mask, x.Left.Add(cmp, hash, key, value), x.Right)
            elif m = 1u then 
                HashMapInner.New(x.Prefix, x.Mask, x.Left, x.Right.Add(cmp, hash, key, value))
            else
                let n = HashMapNoCollisionLeaf.New(hash, key, value)
                HashMapInner.Join(x.Prefix, x, hash, n)

        override x.Alter(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, update: option<'V> -> option<'V>) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                let ll = x.Left.Alter(cmp, hash, key, update)
                if ll == x.Left then x:> _
                else HashMapInner.New(x.Prefix, x.Mask, ll, x.Right)
            elif m = 1u then
                let rr = x.Right.Alter(cmp, hash, key, update)
                if rr == x.Right then x:> _
                else HashMapInner.New(x.Prefix, x.Mask, x.Left, rr)
            else
                match update None with
                | None -> x:> _
                | Some value ->
                    let n = HashMapNoCollisionLeaf.New(hash, key, value)
                    HashMapInner.Join(x.Prefix, x, hash, n)
                    
        override x.AlterV(cmp: EqualityComparer<'K>, hash: uint32, key: 'K, update: voption<'V> -> voption<'V>) =
            let m = matchPrefixAndGetBit hash x.Prefix x.Mask
            if m = 0u then 
                let ll = x.Left.AlterV(cmp, hash, key, update)
                if ll == x.Left then x:> _
                else HashMapInner.New(x.Prefix, x.Mask, ll, x.Right)
            elif m = 1u then
                let rr = x.Right.AlterV(cmp, hash, key, update)
                if rr == x.Right then x:> _
                else HashMapInner.New(x.Prefix, x.Mask, x.Left, rr)
            else
                match update ValueNone with
                | ValueNone -> x:> _
                | ValueSome value ->
                    let n = HashMapNoCollisionLeaf.New(hash, key, value)
                    HashMapInner.Join(x.Prefix, x, hash, n)
                    
        override x.Map(mapping: OptimizedClosures.FSharpFunc<'K, 'V, 'T>) =
            HashMapInner.New(x.Prefix, x.Mask, x.Left.Map(mapping), x.Right.Map(mapping))
  
        override x.Choose(mapping: OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>) =
            HashMapInner.Create(x.Prefix, x.Mask, x.Left.Choose(mapping), x.Right.Choose(mapping))
            
        override x.ChooseV(mapping: OptimizedClosures.FSharpFunc<'K, 'V, ValueOption<'T>>) =
            HashMapInner.Create(x.Prefix, x.Mask, x.Left.ChooseV(mapping), x.Right.ChooseV(mapping))
      
        override x.ChooseV2(mapping: OptimizedClosures.FSharpFunc<'K, 'V, struct(ValueOption<'T1> * ValueOption<'T2>)>) =
            let struct (la, lb) = x.Left.ChooseV2(mapping)
            let struct (ra, rb) = x.Right.ChooseV2(mapping)

            struct (
                HashMapInner.Create(x.Prefix, x.Mask, la, ra),
                HashMapInner.Create(x.Prefix, x.Mask, lb, rb)
            )

        override x.ChooseSV2(mapping: OptimizedClosures.FSharpFunc<'K, 'V, struct(bool * ValueOption<'T2>)>) =
            let struct (la, lb) = x.Left.ChooseSV2(mapping)
            let struct (ra, rb) = x.Right.ChooseSV2(mapping)

            struct (
                HashSetInner.Create(x.Prefix, x.Mask, la, ra),
                HashMapInner.Create(x.Prefix, x.Mask, lb, rb)
            )
      
        override x.Filter(predicate: OptimizedClosures.FSharpFunc<'K, 'V, bool>) =
            HashMapInner.Create(x.Prefix, x.Mask, x.Left.Filter(predicate), x.Right.Filter(predicate))
            
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
            new HashMapInner<_,_>(Prefix = p, Mask = m, Left = l, Right = r, _Count = l.Count + r.Count) :> _

    
    // ========================================================================================================================
    // crazy OO Visitors
    // ========================================================================================================================
    [<AbstractClass>]
    type HashSetVisitor<'T, 'R>() =
        abstract member VisitEmpty : HashSetEmpty<'T> -> 'R
        abstract member VisitNoCollision : HashSetNoCollisionLeaf<'T> -> 'R
        abstract member VisitLeaf : HashSetCollisionLeaf<'T> -> 'R
        abstract member VisitNode : HashSetInner<'T> -> 'R

    [<AbstractClass>]
    type HashMapVisitor<'K, 'V, 'R>() =
        abstract member VisitNode: HashMapInner<'K, 'V> -> 'R
        abstract member VisitLeaf: HashMapCollisionLeaf<'K, 'V> -> 'R
        abstract member VisitNoCollision: HashMapNoCollisionLeaf<'K, 'V> -> 'R
        abstract member VisitEmpty: HashMapEmpty<'K, 'V> -> 'R
        
    [<AbstractClass>]
    type HashMapVisitor2<'K, 'V1, 'V2, 'R>() =
        abstract member VisitNN     : HashMapInner<'K, 'V1> * HashMapInner<'K, 'V2> -> 'R

        abstract member VisitNL     : HashMapInner<'K, 'V1> * HashMapLeaf<'K, 'V2> -> 'R
        abstract member VisitLN     : HashMapLeaf<'K, 'V1> * HashMapInner<'K, 'V2> -> 'R
        abstract member VisitLL     : HashMapLeaf<'K, 'V1> * HashMapLeaf<'K, 'V2> -> 'R

        abstract member VisitAE     : HashMapNode<'K, 'V1> * HashMapEmpty<'K, 'V2> -> 'R
        abstract member VisitEA     : HashMapEmpty<'K, 'V1> * HashMapNode<'K, 'V2> -> 'R
        abstract member VisitEE     : HashMapEmpty<'K, 'V1> * HashMapEmpty<'K, 'V2> -> 'R
  
    [<AbstractClass>]
    type HashSetMapVisitor<'K, 'V, 'R>() =
        abstract member VisitNN     : HashSetInner<'K> * HashMapInner<'K, 'V> -> 'R

        abstract member VisitNL     : HashSetInner<'K> * HashMapLeaf<'K, 'V> -> 'R
        abstract member VisitLN     : HashSetLeaf<'K> * HashMapInner<'K, 'V> -> 'R
        abstract member VisitLL     : HashSetLeaf<'K> * HashMapLeaf<'K, 'V> -> 'R

        abstract member VisitAE     : HashSetNode<'K> * HashMapEmpty<'K, 'V> -> 'R
        abstract member VisitEA     : HashSetEmpty<'K> * HashMapNode<'K, 'V> -> 'R
        abstract member VisitEE     : HashSetEmpty<'K> * HashMapEmpty<'K, 'V> -> 'R
        
    [<AbstractClass>]
    type HashSetVisitor2<'T, 'R>() =
        abstract member VisitNN     : HashSetInner<'T> * HashSetInner<'T> -> 'R

        abstract member VisitNL     : HashSetInner<'T> * HashSetLeaf<'T> -> 'R
        abstract member VisitLN     : HashSetLeaf<'T> * HashSetInner<'T> -> 'R
        abstract member VisitLL     : HashSetLeaf<'T> * HashSetLeaf<'T> -> 'R

        abstract member VisitAE     : HashSetNode<'T> * HashSetEmpty<'T> -> 'R
        abstract member VisitEA     : HashSetEmpty<'T> * HashSetNode<'T> -> 'R
        abstract member VisitEE     : HashSetEmpty<'T> * HashSetEmpty<'T> -> 'R

    type HashMapVisit2Visitor<'K, 'V1, 'V2, 'R>(real : HashMapVisitor2<'K, 'V1, 'V2, 'R>, node : HashMapNode<'K, 'V2>) =
        inherit HashMapVisitor<'K,'V1,'R>()

        override x.VisitLeaf l = 
            node.Accept {
                new HashMapVisitor<'K, 'V2, 'R>() with
                    member x.VisitLeaf r = real.VisitLL(l, r)
                    member x.VisitNode r = real.VisitLN(l, r)
                    member x.VisitNoCollision r = real.VisitLL(l, r)
                    member x.VisitEmpty r = real.VisitAE(l, r)
            }
            
        override x.VisitNode l = 
            node.Accept {
                new HashMapVisitor<'K, 'V2, 'R>() with
                    member x.VisitLeaf r = real.VisitNL(l, r)
                    member x.VisitNode r = real.VisitNN(l, r)
                    member x.VisitNoCollision r = real.VisitNL(l, r)
                    member x.VisitEmpty r = real.VisitAE(l, r)
            }
            
        override x.VisitNoCollision l = 
            node.Accept {
                new HashMapVisitor<'K, 'V2, 'R>() with
                    member x.VisitLeaf r = real.VisitLL(l, r)
                    member x.VisitNode r = real.VisitLN(l, r)
                    member x.VisitNoCollision r = real.VisitLL(l, r)
                    member x.VisitEmpty r = real.VisitAE(l, r)
            }
            
        override x.VisitEmpty l = 
            node.Accept {
                new HashMapVisitor<'K, 'V2, 'R>() with
                    member x.VisitLeaf r = real.VisitEA(l, r)
                    member x.VisitNode r = real.VisitEA(l, r)
                    member x.VisitNoCollision r = real.VisitEA(l, r)
                    member x.VisitEmpty r = real.VisitEE(l, r)
            }
            
    type HashSetVisit2Visitor<'T, 'R>(real : HashSetVisitor2<'T, 'R>, node : HashSetNode<'T>) =
        inherit HashSetVisitor<'T,'R>()

        override x.VisitLeaf l = 
            node.Accept {
                new HashSetVisitor<'T, 'R>() with
                    member x.VisitLeaf r = real.VisitLL(l, r)
                    member x.VisitNode r = real.VisitLN(l, r)
                    member x.VisitNoCollision r = real.VisitLL(l, r)
                    member x.VisitEmpty r = real.VisitAE(l, r)
            }
            
        override x.VisitNode l = 
            node.Accept {
                new HashSetVisitor<'T, 'R>() with
                    member x.VisitLeaf r = real.VisitNL(l, r)
                    member x.VisitNode r = real.VisitNN(l, r)
                    member x.VisitNoCollision r = real.VisitNL(l, r)
                    member x.VisitEmpty r = real.VisitAE(l, r)
            }
            
        override x.VisitNoCollision l = 
            node.Accept {
                new HashSetVisitor<'T, 'R>() with
                    member x.VisitLeaf r = real.VisitLL(l, r)
                    member x.VisitNode r = real.VisitLN(l, r)
                    member x.VisitNoCollision r = real.VisitLL(l, r)
                    member x.VisitEmpty r = real.VisitAE(l, r)
            }
            
        override x.VisitEmpty l = 
            node.Accept {
                new HashSetVisitor<'T, 'R>() with
                    member x.VisitLeaf r = real.VisitEA(l, r)
                    member x.VisitNode r = real.VisitEA(l, r)
                    member x.VisitNoCollision r = real.VisitEA(l, r)
                    member x.VisitEmpty r = real.VisitEE(l, r)
            }
            
    type HashMapSetVisit2Visitor<'K, 'V, 'R>(real : HashSetMapVisitor<'K, 'V, 'R>, node : HashMapNode<'K, 'V>) =
        inherit HashSetVisitor<'K,'R>()

        override x.VisitLeaf l = 
            node.Accept {
                new HashMapVisitor<'K, 'V, 'R>() with
                    member x.VisitLeaf r = real.VisitLL(l, r)
                    member x.VisitNode r = real.VisitLN(l, r)
                    member x.VisitNoCollision r = real.VisitLL(l, r)
                    member x.VisitEmpty r = real.VisitAE(l, r)
            }
            
        override x.VisitNode l = 
            node.Accept {
                new HashMapVisitor<'K, 'V, 'R>() with
                    member x.VisitLeaf r = real.VisitNL(l, r)
                    member x.VisitNode r = real.VisitNN(l, r)
                    member x.VisitNoCollision r = real.VisitNL(l, r)
                    member x.VisitEmpty r = real.VisitAE(l, r)
            }
            
        override x.VisitNoCollision l = 
            node.Accept {
                new HashMapVisitor<'K, 'V, 'R>() with
                    member x.VisitLeaf r = real.VisitLL(l, r)
                    member x.VisitNode r = real.VisitLN(l, r)
                    member x.VisitNoCollision r = real.VisitLL(l, r)
                    member x.VisitEmpty r = real.VisitAE(l, r)
            }
            
        override x.VisitEmpty l = 
            node.Accept {
                new HashMapVisitor<'K, 'V, 'R>() with
                    member x.VisitLeaf r = real.VisitEA(l, r)
                    member x.VisitNode r = real.VisitEA(l, r)
                    member x.VisitNoCollision r = real.VisitEA(l, r)
                    member x.VisitEmpty r = real.VisitEE(l, r)
            }

    module HashMapNode = 
        let visit2 (v : HashMapVisitor2<'K, 'V1, 'V2, 'R>) (l : HashMapNode<'K, 'V1>) (r : HashMapNode<'K, 'V2>) =
            l.Accept (HashMapVisit2Visitor(v, r))
            
        let computeDelta 
            (cmp : EqualityComparer<'K>)
            (add : 'K -> 'V -> 'OP)
            (update : 'K -> 'V -> 'V -> ValueOption<'OP>)
            (remove : 'K -> 'V -> 'OP)
            (l : HashMapNode<'K, 'V>) 
            (r : HashMapNode<'K, 'V>)  =
            let add = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(add)
            let remove = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(remove)
            let update = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(update)

        
            let len = ref 0
            let arr = ref (Array.zeroCreate 4)

            (l, r) ||> HashMapNode.visit2 {
                new HashMapVisitor2<'K, 'V, 'V, HashMapNode<'K, 'OP>>() with

                    member x.VisitEE(_, _) = HashMapEmpty.Instance
                    member x.VisitEA(_, r) = r.Map(add)
                    member x.VisitAE(l, _) = l.Map(remove)

                    member x.VisitLL(l, r) = 
                        if l == r then
                            HashMapEmpty.Instance
                        else
                            len := 0
                            if l.LHash = r.LHash then
                                let mutable r = r :> HashMapNode<_,_>
                                let mutable res = HashMapEmpty.Instance
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
                            HashMapInner.Create(r.Prefix, r.Mask, HashMapNode.visit2 x l r.Left, r.Right.Map(add))
                        elif b = 1u then
                            HashMapInner.Create(r.Prefix, r.Mask, r.Left.Map(add), HashMapNode.visit2 x l r.Right)
                        else
                            HashMapInner.Join(l.LHash, l.Map(remove), r.Prefix, r.Map(add))

                    member x.VisitNL(l, r) =
                        let b = matchPrefixAndGetBit r.LHash l.Prefix l.Mask
                        if b = 0u then
                            HashMapInner.Create(l.Prefix, l.Mask, HashMapNode.visit2 x l.Left r, l.Right.Map(remove))
                        elif b = 1u then
                            HashMapInner.Create(l.Prefix, l.Mask, l.Left.Map(remove), HashMapNode.visit2 x l.Right r)
                        else
                            HashMapInner.Join(l.Prefix, l.Map(remove), r.LHash, r.Map(add))

                    member x.VisitNN(l, r) = 
                        if l == r then
                            HashMapEmpty.Instance
                        else
                            let cc = compareMasks l.Mask r.Mask
                            if cc = 0 then
                                let l' = (l.Left, r.Left) ||> HashMapNode.visit2 x
                                let r' = (l.Right, r.Right) ||> HashMapNode.visit2 x
                                HashMapInner.Create(l.Prefix, l.Mask, l', r')
                            elif cc > 0 then
                                let lr = matchPrefixAndGetBit l.Prefix r.Prefix r.Mask
                                if lr = 0u then
                                    HashMapInner.Create(r.Prefix, r.Mask, HashMapNode.visit2 x l r.Left, r.Right.Map(add))
                                elif lr = 1u then
                                    HashMapInner.Create(r.Prefix, r.Mask, r.Left.Map(add), HashMapNode.visit2 x l r.Right)
                                else
                                    HashMapInner.Join(l.Prefix, l.Map(remove), r.Prefix, r.Map(add))
                            else
                                let rl = matchPrefixAndGetBit r.Prefix l.Prefix l.Mask
                            
                                if rl = 0u then
                                    HashMapInner.Create(l.Prefix, l.Mask, HashMapNode.visit2 x l.Left r, l.Right.Map(remove))
                                elif rl = 1u then
                                    HashMapInner.Create(l.Prefix, l.Mask, l.Left.Map(remove), HashMapNode.visit2 x l.Right r)
                                else
                                    HashMapInner.Join(l.Prefix, l.Map(remove), r.Prefix, r.Map(add))
                                    
            }

        let unionWith
            (cmp : EqualityComparer<'K>)
            (resolve : 'K -> 'V -> 'V -> 'V)
            (l : HashMapNode<'K, 'V>)
            (r : HashMapNode<'K, 'V>) =
            let resolve = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(resolve)

            let len = ref 0
            let arr = ref (Array.zeroCreate 4)

            (l, r) ||> HashMapNode.visit2 {
                new HashMapVisitor2<'K, 'V, 'V, HashMapNode<'K, 'V>>() with

                    member x.VisitEE(_, _) = HashMapEmpty.Instance
                    member x.VisitEA(_, r) = r
                    member x.VisitAE(l, _) = l

                    member x.VisitLL(l, r) = 
                        len := 0
                        if l.LHash = r.LHash then
                            let mutable r = r :> HashMapNode<_,_>
                            let mutable res = HashMapEmpty.Instance
                            let hash = l.LHash
                    
                            l.ToArray(arr, len)
                            for i in 0 .. !len - 1 do
                                let struct (k, lv) = arr.Value.[i]
                                match r.TryRemove(cmp, hash, k) with
                                | ValueSome (rv, rest) ->
                                    r <- rest
                                    let op = resolve.Invoke(k, lv, rv)
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
                            HashMapInner.Join(l.LHash, l, r.LHash, r)
                         

                    member x.VisitLN(l, r) =
                        let b = matchPrefixAndGetBit l.LHash r.Prefix r.Mask
                        if b = 0u then
                            HashMapInner.Create(r.Prefix, r.Mask, HashMapNode.visit2 x l r.Left, r.Right)
                        elif b = 1u then
                            HashMapInner.Create(r.Prefix, r.Mask, r.Left, HashMapNode.visit2 x l r.Right)
                        else
                            HashMapInner.Join(l.LHash, l, r.Prefix, r)

                    member x.VisitNL(l, r) =
                        let b = matchPrefixAndGetBit r.LHash l.Prefix l.Mask
                        if b = 0u then
                            HashMapInner.Create(l.Prefix, l.Mask, HashMapNode.visit2 x l.Left r, l.Right)
                        elif b = 1u then
                            HashMapInner.Create(l.Prefix, l.Mask, l.Left, HashMapNode.visit2 x l.Right r)
                        else
                            HashMapInner.Join(l.Prefix, l, r.LHash, r)

                    member x.VisitNN(l, r) = 
                        let cc = compareMasks l.Mask r.Mask
                        if cc = 0 then
                            let l' = (l.Left, r.Left) ||> HashMapNode.visit2 x
                            let r' = (l.Right, r.Right) ||> HashMapNode.visit2 x
                            HashMapInner.Create(l.Prefix, l.Mask, l', r')
                        elif cc > 0 then
                            let lr = matchPrefixAndGetBit l.Prefix r.Prefix r.Mask
                            if lr = 0u then
                                HashMapInner.Create(r.Prefix, r.Mask, HashMapNode.visit2 x l r.Left, r.Right)
                            elif lr = 1u then
                                HashMapInner.Create(r.Prefix, r.Mask, r.Left, HashMapNode.visit2 x l r.Right)
                            else
                                HashMapInner.Join(l.Prefix, l, r.Prefix, r)
                        else
                            let rl = matchPrefixAndGetBit r.Prefix l.Prefix l.Mask
                        
                            if rl = 0u then
                                HashMapInner.Create(l.Prefix, l.Mask, HashMapNode.visit2 x l.Left r, l.Right)
                            elif rl = 1u then
                                HashMapInner.Create(l.Prefix, l.Mask, l.Left, HashMapNode.visit2 x l.Right r)
                            else
                                HashMapInner.Join(l.Prefix, l, r.Prefix, r)
                                
            }

        let union
            (cmp : EqualityComparer<'K>) 
            (l : HashMapNode<'K, 'V>) 
            (r : HashMapNode<'K, 'V>) =
            let len = ref 0
            let arr = ref (Array.zeroCreate 4)
            (l, r) ||> HashMapNode.visit2 {
                new HashMapVisitor2<'K, 'V, 'V, HashMapNode<'K, 'V>>() with

                    member x.VisitEE(_, _) = HashMapEmpty.Instance
                    member x.VisitEA(_, r) = r
                    member x.VisitAE(l, _) = l

                    member x.VisitLL(l, r) = 
                        if l == r then
                            r :> HashMapNode<_,_>
                        else
                            len := 0
                            if l.LHash = r.LHash then
                                let mutable r = r :> HashMapNode<_,_>
                                let mutable res = HashMapEmpty.Instance
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
                                HashMapInner.Join(l.LHash, l, r.LHash, r)
                     

                    member x.VisitLN(l, r) =
                        let b = matchPrefixAndGetBit l.LHash r.Prefix r.Mask
                        if b = 0u then
                            HashMapInner.Create(r.Prefix, r.Mask, HashMapNode.visit2 x l r.Left, r.Right)
                        elif b = 1u then
                            HashMapInner.Create(r.Prefix, r.Mask, r.Left, HashMapNode.visit2 x l r.Right)
                        else
                            HashMapInner.Join(l.LHash, l, r.Prefix, r)

                    member x.VisitNL(l, r) =
                        let b = matchPrefixAndGetBit r.LHash l.Prefix l.Mask
                        if b = 0u then
                            HashMapInner.Create(l.Prefix, l.Mask, HashMapNode.visit2 x l.Left r, l.Right)
                        elif b = 1u then
                            HashMapInner.Create(l.Prefix, l.Mask, l.Left, HashMapNode.visit2 x l.Right r)
                        else
                            HashMapInner.Join(l.Prefix, l, r.LHash, r)

                    member x.VisitNN(l, r) = 
                        if l == r then 
                            r :> HashMapNode<_,_>
                        else
                            let cc = compareMasks l.Mask r.Mask
                            if cc = 0 then
                                let l' = (l.Left, r.Left) ||> HashMapNode.visit2 x
                                let r' = (l.Right, r.Right) ||> HashMapNode.visit2 x
                                HashMapInner.Create(l.Prefix, l.Mask, l', r')
                            elif cc > 0 then
                                let lr = matchPrefixAndGetBit l.Prefix r.Prefix r.Mask
                                if lr = 0u then
                                    HashMapInner.Create(r.Prefix, r.Mask, HashMapNode.visit2 x l r.Left, r.Right)
                                elif lr = 1u then
                                    HashMapInner.Create(r.Prefix, r.Mask, r.Left, HashMapNode.visit2 x l r.Right)
                                else
                                    HashMapInner.Join(l.Prefix, l, r.Prefix, r)
                            else
                                let rl = matchPrefixAndGetBit r.Prefix l.Prefix l.Mask
                    
                                if rl = 0u then
                                    HashMapInner.Create(l.Prefix, l.Mask, HashMapNode.visit2 x l.Left r, l.Right)
                                elif rl = 1u then
                                    HashMapInner.Create(l.Prefix, l.Mask, l.Left, HashMapNode.visit2 x l.Right r)
                                else
                                    HashMapInner.Join(l.Prefix, l, r.Prefix, r)
                            
            }

        let applyDelta
            (cmp : EqualityComparer<'K>) 
            (apply : 'K -> voption<'V> -> 'D -> struct(voption<'V> * voption<'D>))
            (state : HashMapNode<'K, 'V>)
            (delta : HashMapNode<'K, 'D>) =

            let len = ref 0
            let arr1 = ref (Array.zeroCreate 4)
            let arr2 = ref (Array.zeroCreate 4)
            let apply = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(apply)
            let onlyDelta = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(fun k d -> apply.Invoke(k, ValueNone, d))
    
            (state, delta) ||> HashMapNode.visit2 {
                new HashMapVisitor2<'K, 'V, 'D, struct (HashMapNode<'K, 'V> * HashMapNode<'K, 'D>)>() with

                    member x.VisitEE(_, _) = 
                        struct (HashMapEmpty.Instance, HashMapEmpty.Instance)

                    member x.VisitEA(_, r) =    
                        r.ChooseV2 onlyDelta

                    member x.VisitAE(l, _) = 
                        struct(l, HashMapEmpty.Instance)

                    member x.VisitLL(state, delta) = 
                        len := 0
                        if state.LHash = delta.LHash then
                            let mutable delta = delta :> HashMapNode<_,_>
                            let mutable resState = HashMapEmpty.Instance
                            let mutable resDelta = HashMapEmpty.Instance
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
                                HashMapInner.Join(state.LHash, state, delta.LHash, ds),
                                dd
                            )

                    member x.VisitLN(state, delta) =
                        let b = matchPrefixAndGetBit state.LHash delta.Prefix delta.Mask
                        if b = 0u then
                            let struct (ls, ld) = HashMapNode.visit2 x state delta.Left
                            let struct (rs, rd) = delta.Right.ChooseV2(onlyDelta)
                            struct(
                                HashMapInner.Create(delta.Prefix, delta.Mask, ls, rs),
                                HashMapInner.Create(delta.Prefix, delta.Mask, ld, rd)
                            )
                        elif b = 1u then
                            let struct (ls, ld) = delta.Left.ChooseV2(onlyDelta)
                            let struct (rs, rd) = HashMapNode.visit2 x state delta.Right
                            struct(
                                HashMapInner.Create(delta.Prefix, delta.Mask, ls, rs),
                                HashMapInner.Create(delta.Prefix, delta.Mask, ld, rd)
                            )
                        else
                            let struct (ds, dd) = delta.ChooseV2(onlyDelta)
                            struct(
                                HashMapInner.Join(state.LHash, state, delta.Prefix, ds),
                                dd
                            )

                    member x.VisitNL(l, r) =
                        let b = matchPrefixAndGetBit r.LHash l.Prefix l.Mask
                        if b = 0u then
                            let struct (ls, ld) = HashMapNode.visit2 x l.Left r
                            struct (
                                HashMapInner.Create(l.Prefix, l.Mask, ls, l.Right),
                                ld
                            )
                        elif b = 1u then
                            let struct (rs, rd) = HashMapNode.visit2 x l.Right r
                            struct (
                                HashMapInner.Create(l.Prefix, l.Mask, l.Left, rs),
                                rd
                            )
                        else
                            let struct (rs, rd) = r.ChooseV2(onlyDelta)
                            struct (
                                HashMapInner.Join(l.Prefix, l, r.LHash, rs),
                                rd
                            )

                    member x.VisitNN(l, r) = 
                        let cc = compareMasks l.Mask r.Mask
                        if cc = 0 then
                            let struct (ls, ld) = (l.Left, r.Left) ||> HashMapNode.visit2 x
                            let struct (rs, rd) = (l.Right, r.Right) ||> HashMapNode.visit2 x
                            struct (
                                HashMapInner.Create(l.Prefix, l.Mask, ls, rs),
                                HashMapInner.Create(l.Prefix, l.Mask, ld, rd)
                            )
                        elif cc > 0 then
                            let lr = matchPrefixAndGetBit l.Prefix r.Prefix r.Mask
                            if lr = 0u then
                                let struct (ls, ld) = HashMapNode.visit2 x l r.Left
                                let struct (rs, rd) = r.Right.ChooseV2(onlyDelta)
                                struct (
                                    HashMapInner.Create(r.Prefix, r.Mask, ls, rs),
                                    HashMapInner.Create(r.Prefix, r.Mask, ld, rd)
                                )
                            elif lr = 1u then
                                let struct (ls, ld) = r.Left.ChooseV2(onlyDelta)
                                let struct (rs, rd) = HashMapNode.visit2 x l r.Right
                                struct (
                                    HashMapInner.Create(r.Prefix, r.Mask, ls, rs),
                                    HashMapInner.Create(r.Prefix, r.Mask, ld, rd)
                                )
                            else
                                let struct (rs, rd) = r.ChooseV2 onlyDelta
                                struct (
                                    HashMapInner.Join(l.Prefix, l, r.Prefix, rs),
                                    rd
                                )
                        else
                            let rl = matchPrefixAndGetBit r.Prefix l.Prefix l.Mask
                        
                            if rl = 0u then
                                let struct (ls, ld) = HashMapNode.visit2 x l.Left r
                                struct (
                                    HashMapInner.Create(l.Prefix, l.Mask, ls, l.Right),
                                    ld
                                )
                            elif rl = 1u then
                                let struct (rs, rd) = HashMapNode.visit2 x l.Right r
                                struct (
                                    HashMapInner.Create(l.Prefix, l.Mask, l.Left, rs),
                                    rd
                                )
                            else
                                let struct (rs, rd) = r.ChooseV2 onlyDelta
                                struct (
                                    HashMapInner.Join(l.Prefix, l, r.Prefix, rs),
                                    rd
                                )
                                
            }


    module HashSetNode = 
        let visit2 (v : HashSetVisitor2<'T, 'R>) (l : HashSetNode<'T>) (r : HashSetNode<'T>) =
            l.Accept (HashSetVisit2Visitor(v, r))

        let visitMap2 (v : HashSetMapVisitor<'K, 'V, 'R>) (l : HashSetNode<'K>) (r : HashMapNode<'K, 'V>) =
            l.Accept (HashMapSetVisit2Visitor(v, r))

        
        let union 
            (cmp : EqualityComparer<'T>)
            (l : HashSetNode<'T>) 
            (r : HashSetNode<'T>)  =

            let len = ref 0
            let arr = ref (Array.zeroCreate 4)

            (l, r) ||> visit2 {
                new HashSetVisitor2<'T, HashSetNode<'T>>() with

                    member x.VisitEE(_, _) = HashSetEmpty.Instance
                    member x.VisitEA(_, r) = r
                    member x.VisitAE(l, _) = l

                    member x.VisitLL(l, r) = 
                        if l == r then
                            r :> HashSetNode<_>
                        else
                            len := 0
                            if l.LHash = r.LHash then
                                let mutable r = r :> HashSetNode<_>
                                let hash = l.LHash
                                l.ToArray(arr, len)
                                for i in 0 .. !len - 1 do
                                    let lv = arr.Value.[i]
                                    r <- r.Add(cmp, hash, lv)
                                r
                            else
                                HashSetInner.Join(l.LHash, l, r.LHash, r)

                    member x.VisitLN(l, r) =
                        let b = matchPrefixAndGetBit l.LHash r.Prefix r.Mask
                        if b = 0u then
                            HashSetInner.Create(r.Prefix, r.Mask, visit2 x l r.Left, r.Right)
                        elif b = 1u then
                            HashSetInner.Create(r.Prefix, r.Mask, r.Left, visit2 x l r.Right)
                        else
                            HashSetInner.Join(l.LHash, l, r.Prefix, r)

                    member x.VisitNL(l, r) =
                        let b = matchPrefixAndGetBit r.LHash l.Prefix l.Mask
                        if b = 0u then
                            HashSetInner.Create(l.Prefix, l.Mask, visit2 x l.Left r, l.Right)
                        elif b = 1u then
                            HashSetInner.Create(l.Prefix, l.Mask, l.Left, visit2 x l.Right r)
                        else
                            HashSetInner.Join(l.Prefix, l, r.LHash, r)

                    member x.VisitNN(l, r) = 
                        if l == r then
                            r :> HashSetNode<_>
                        else
                            let cc = compareMasks l.Mask r.Mask
                            if cc = 0 then
                                let l' = (l.Left, r.Left) ||> visit2 x
                                let r' = (l.Right, r.Right) ||> visit2 x
                                HashSetInner.Create(l.Prefix, l.Mask, l', r')
                            elif cc > 0 then
                                let lr = matchPrefixAndGetBit l.Prefix r.Prefix r.Mask
                                if lr = 0u then
                                    HashSetInner.Create(r.Prefix, r.Mask, visit2 x l r.Left, r.Right)
                                elif lr = 1u then
                                    HashSetInner.Create(r.Prefix, r.Mask, r.Left, visit2 x l r.Right)
                                else
                                    HashSetInner.Join(l.Prefix, l, r.Prefix, r)
                            else
                                let rl = matchPrefixAndGetBit r.Prefix l.Prefix l.Mask
                            
                                if rl = 0u then
                                    HashSetInner.Create(l.Prefix, l.Mask, visit2 x l.Left r, l.Right)
                                elif rl = 1u then
                                    HashSetInner.Create(l.Prefix, l.Mask, l.Left, visit2 x l.Right r)
                                else
                                    HashSetInner.Join(l.Prefix, l, r.Prefix, r)
                                    
            }
            
        let difference 
            (cmp : EqualityComparer<'T>)
            (l : HashSetNode<'T>) 
            (r : HashSetNode<'T>)  =

            let len = ref 0
            let arr = ref (Array.zeroCreate 4)

            (l, r) ||> visit2 {
                new HashSetVisitor2<'T, HashSetNode<'T>>() with

                    member x.VisitEE(_, _) = HashSetEmpty.Instance
                    member x.VisitEA(_, _) = HashSetEmpty.Instance
                    member x.VisitAE(l, _) = l

                    member x.VisitLL(l, r) = 
                        if l == r then
                            HashSetEmpty.Instance
                        else
                            len := 0
                            if l.LHash = r.LHash then
                                let mutable l = l :> HashSetNode<_>
                                let hash = r.LHash
                                r.ToArray(arr, len)
                                for i in 0 .. !len - 1 do
                                    let lv = arr.Value.[i]
                                    l <- l.Remove(cmp, hash, lv)
                                l
                            else
                                l :> HashSetNode<_>

                    member x.VisitLN(l, r) =
                        let rest = l.LNext |> HashSetLinked.filter (fun v -> not (r.Contains(cmp, l.LHash, v)))
                        if not (r.Contains(cmp, l.LHash, l.LValue)) then 
                            HashSetLeaf.New(l.LHash, l.LValue, rest)
                        else
                            match HashSetLinked.destruct rest with
                            | ValueSome (struct (v, rest)) ->
                                HashSetLeaf.New(l.LHash, v, rest)
                            | ValueNone ->
                                HashSetEmpty.Instance

                    member x.VisitNL(l, r) =
                        let b = matchPrefixAndGetBit r.LHash l.Prefix l.Mask
                        if b = 0u then
                            HashSetInner.Create(l.Prefix, l.Mask, visit2 x l.Left r, l.Right)
                        elif b = 1u then
                            HashSetInner.Create(l.Prefix, l.Mask, l.Left, visit2 x l.Right r)
                        else
                            l :> HashSetNode<_>

                    member x.VisitNN(l, r) = 
                        if l == r then
                            HashSetEmpty.Instance
                        else
                            let cc = compareMasks l.Mask r.Mask
                            if cc = 0 then
                                let l' = (l.Left, r.Left) ||> visit2 x
                                let r' = (l.Right, r.Right) ||> visit2 x
                                HashSetInner.Create(l.Prefix, l.Mask, l', r')
                            elif cc > 0 then
                                let lr = matchPrefixAndGetBit l.Prefix r.Prefix r.Mask
                                if lr = 0u then
                                    HashSetInner.Create(r.Prefix, r.Mask, visit2 x l r.Left, r.Right)
                                elif lr = 1u then
                                    HashSetInner.Create(r.Prefix, r.Mask, r.Left, visit2 x l r.Right)
                                else
                                    l :> HashSetNode<_>
                            else
                                let rl = matchPrefixAndGetBit r.Prefix l.Prefix l.Mask
                            
                                if rl = 0u then
                                    HashSetInner.Create(l.Prefix, l.Mask, visit2 x l.Left r, l.Right)
                                elif rl = 1u then
                                    HashSetInner.Create(l.Prefix, l.Mask, l.Left, visit2 x l.Right r)
                                else
                                    l :> HashSetNode<_>
                                    
            }


        let computeDelta 
            (cmp : EqualityComparer<'T>)
            (add : 'T -> 'OP)
            (remove : 'T -> 'OP)
            (l : HashSetNode<'T>) 
            (r : HashSetNode<'T>)  =

            let len = ref 0
            let arr = ref (Array.zeroCreate 4)

            (l, r) ||> visit2 {
                new HashSetVisitor2<'T, HashMapNode<'T, 'OP>>() with

                    member x.VisitEE(_, _) = HashMapEmpty.Instance
                    member x.VisitEA(_, r) = r.MapToMap(add)
                    member x.VisitAE(l, _) = l.MapToMap(remove)

                    member x.VisitLL(l, r) = 
                        if l == r then
                            HashMapEmpty.Instance
                        else
                            len := 0
                            if l.LHash = r.LHash then
                                let mutable r = r :> HashSetNode<_>
                                let mutable res = HashMapEmpty.Instance
                                let hash = l.LHash
                        
                                l.ToArray(arr, len)
                                for i in 0 .. !len - 1 do
                                    let lv = arr.Value.[i]
                                    match r.TryRemove(cmp, hash, lv) with
                                    | ValueSome rest ->
                                        r <- rest
                                    | ValueNone ->
                                        res <- res.AddInPlaceUnsafe(cmp, hash, lv, remove lv)

                                len := 0
                                r.ToArray(arr, len)
                                for i in 0 .. !len - 1 do
                                    let rv = arr.Value.[i]
                                    res <- res.AddInPlaceUnsafe(cmp, hash, rv, add rv)
                        
                                res
                            else
                                let mutable res = l.MapToMap(remove)
                                r.ToArray(arr, len)
                                for i in 0 .. !len - 1 do
                                    let rv = arr.Value.[i]
                                    res <- res.AddInPlaceUnsafe(cmp, r.LHash, rv, add rv)
                                res

                    member x.VisitLN(l, r) =
                        let b = matchPrefixAndGetBit l.LHash r.Prefix r.Mask
                        if b = 0u then
                            HashMapInner.Create(r.Prefix, r.Mask, visit2 x l r.Left, r.Right.MapToMap(add))
                        elif b = 1u then
                            HashMapInner.Create(r.Prefix, r.Mask, r.Left.MapToMap(add), visit2 x l r.Right)
                        else
                            HashMapInner.Join(l.LHash, l.MapToMap(remove), r.Prefix, r.MapToMap(add))

                    member x.VisitNL(l, r) =
                        let b = matchPrefixAndGetBit r.LHash l.Prefix l.Mask
                        if b = 0u then
                            HashMapInner.Create(l.Prefix, l.Mask, visit2 x l.Left r, l.Right.MapToMap(remove))
                        elif b = 1u then
                            HashMapInner.Create(l.Prefix, l.Mask, l.Left.MapToMap(remove), visit2 x l.Right r)
                        else
                            HashMapInner.Join(l.Prefix, l.MapToMap(remove), r.LHash, r.MapToMap(add))

                    member x.VisitNN(l, r) = 
                        if l == r then
                            HashMapEmpty.Instance
                        else
                            let cc = compareMasks l.Mask r.Mask
                            if cc = 0 then
                                let l' = (l.Left, r.Left) ||> visit2 x
                                let r' = (l.Right, r.Right) ||> visit2 x
                                HashMapInner.Create(l.Prefix, l.Mask, l', r')
                            elif cc > 0 then
                                let lr = matchPrefixAndGetBit l.Prefix r.Prefix r.Mask
                                if lr = 0u then
                                    HashMapInner.Create(r.Prefix, r.Mask, visit2 x l r.Left, r.Right.MapToMap(add))
                                elif lr = 1u then
                                    HashMapInner.Create(r.Prefix, r.Mask, r.Left.MapToMap(add), visit2 x l r.Right)
                                else
                                    HashMapInner.Join(l.Prefix, l.MapToMap(remove), r.Prefix, r.MapToMap(add))
                            else
                                let rl = matchPrefixAndGetBit r.Prefix l.Prefix l.Mask
                            
                                if rl = 0u then
                                    HashMapInner.Create(l.Prefix, l.Mask, visit2 x l.Left r, l.Right.MapToMap(remove))
                                elif rl = 1u then
                                    HashMapInner.Create(l.Prefix, l.Mask, l.Left.MapToMap(remove), visit2 x l.Right r)
                                else
                                    HashMapInner.Join(l.Prefix, l.MapToMap(remove), r.Prefix, r.MapToMap(add))
                                    
            }

        let applyDelta
            (cmp : EqualityComparer<'T>) 
            (apply : 'T -> bool -> 'D -> struct(bool * voption<'D>))
            (state : HashSetNode<'T>)
            (delta : HashMapNode<'T, 'D>) =

            let len = ref 0
            let arr1 = ref (Array.zeroCreate 4)
            let arr2 = ref (Array.zeroCreate 4)
            let apply = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(apply)
            let onlyDelta = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(fun k d -> apply.Invoke(k, false, d))
    
            (state, delta) ||> visitMap2 {
                new HashSetMapVisitor<'T, 'D, struct (HashSetNode<'T> * HashMapNode<'T, 'D>)>() with

                    member x.VisitEE(_, _) = 
                        struct (HashSetEmpty.Instance, HashMapEmpty.Instance)

                    member x.VisitEA(_, r) =    
                        r.ChooseSV2 onlyDelta

                    member x.VisitAE(l, _) = 
                        struct(l, HashMapEmpty.Instance)

                    member x.VisitLL(state, delta) = 
                        len := 0
                        if state.LHash = delta.LHash then
                            let mutable delta = delta :> HashMapNode<_,_>
                            let mutable resState = HashSetEmpty.Instance
                            let mutable resDelta = HashMapEmpty.Instance
                            let hash = state.LHash
                    
                            state.ToArray(arr1, len)
                            for i in 0 .. !len - 1 do
                                let k = arr1.Value.[i]
                                match delta.TryRemove(cmp, hash, k) with
                                | ValueSome (dd, rest) ->
                                    delta <- rest
                                    let struct (s, d) = apply.Invoke(k, true, dd)

                                    if s then 
                                        resState <- resState.AddInPlaceUnsafe(cmp, hash, k)

                                    match d with
                                    | ValueSome v -> resDelta <- resDelta.AddInPlaceUnsafe(cmp, hash, k, v)
                                    | ValueNone -> ()

                                | ValueNone ->
                                    resState <- resState.AddInPlaceUnsafe(cmp, hash, k)

                            len := 0
                            delta.ToArray(arr2, len)
                            for i in 0 .. !len - 1 do
                                let struct (k, rv) = arr2.Value.[i]
                                let struct (s, d) = onlyDelta.Invoke(k, rv)
                                if s then
                                    resState <- resState.AddInPlaceUnsafe(cmp, hash, k)

                                match d with
                                | ValueSome v -> resDelta <- resDelta.AddInPlaceUnsafe(cmp, hash, k, v)
                                | ValueNone -> ()
                    
                            struct(resState, resDelta)
                        else
                            let struct (ds, dd) = delta.ChooseSV2(onlyDelta)
                            struct (
                                HashSetInner.Join(state.LHash, state, delta.LHash, ds),
                                dd
                            )

                    member x.VisitLN(state, delta) =
                        let b = matchPrefixAndGetBit state.LHash delta.Prefix delta.Mask
                        if b = 0u then
                            let struct (ls, ld) = visitMap2 x state delta.Left
                            let struct (rs, rd) = delta.Right.ChooseSV2(onlyDelta)
                            struct(
                                HashSetInner.Create(delta.Prefix, delta.Mask, ls, rs),
                                HashMapInner.Create(delta.Prefix, delta.Mask, ld, rd)
                            )
                        elif b = 1u then
                            let struct (ls, ld) = delta.Left.ChooseSV2(onlyDelta)
                            let struct (rs, rd) = visitMap2 x state delta.Right
                            struct(
                                HashSetInner.Create(delta.Prefix, delta.Mask, ls, rs),
                                HashMapInner.Create(delta.Prefix, delta.Mask, ld, rd)
                            )
                        else
                            let struct (ds, dd) = delta.ChooseSV2(onlyDelta)
                            struct(
                                HashSetInner.Join(state.LHash, state, delta.Prefix, ds),
                                dd
                            )

                    member x.VisitNL(l, r) =
                        let b = matchPrefixAndGetBit r.LHash l.Prefix l.Mask
                        if b = 0u then
                            let struct (ls, ld) = visitMap2 x l.Left r
                            struct (
                                HashSetInner.Create(l.Prefix, l.Mask, ls, l.Right),
                                ld
                            )
                        elif b = 1u then
                            let struct (rs, rd) = visitMap2 x l.Right r
                            struct (
                                HashSetInner.Create(l.Prefix, l.Mask, l.Left, rs),
                                rd
                            )
                        else
                            let struct (rs, rd) = r.ChooseSV2(onlyDelta)
                            struct (
                                HashSetInner.Join(l.Prefix, l, r.LHash, rs),
                                rd
                            )

                    member x.VisitNN(l, r) = 
                        let cc = compareMasks l.Mask r.Mask
                        if cc = 0 then
                            let struct (ls, ld) = (l.Left, r.Left) ||> visitMap2 x
                            let struct (rs, rd) = (l.Right, r.Right) ||> visitMap2 x
                            struct (
                                HashSetInner.Create(l.Prefix, l.Mask, ls, rs),
                                HashMapInner.Create(l.Prefix, l.Mask, ld, rd)
                            )
                        elif cc > 0 then
                            let lr = matchPrefixAndGetBit l.Prefix r.Prefix r.Mask
                            if lr = 0u then
                                let struct (ls, ld) = visitMap2 x l r.Left
                                let struct (rs, rd) = r.Right.ChooseSV2(onlyDelta)
                                struct (
                                    HashSetInner.Create(r.Prefix, r.Mask, ls, rs),
                                    HashMapInner.Create(r.Prefix, r.Mask, ld, rd)
                                )
                            elif lr = 1u then
                                let struct (ls, ld) = r.Left.ChooseSV2(onlyDelta)
                                let struct (rs, rd) = visitMap2 x l r.Right
                                struct (
                                    HashSetInner.Create(r.Prefix, r.Mask, ls, rs),
                                    HashMapInner.Create(r.Prefix, r.Mask, ld, rd)
                                )
                            else
                                let struct (rs, rd) = r.ChooseSV2 onlyDelta
                                struct (
                                    HashSetInner.Join(l.Prefix, l, r.Prefix, rs),
                                    rd
                                )
                        else
                            let rl = matchPrefixAndGetBit r.Prefix l.Prefix l.Mask
                        
                            if rl = 0u then
                                let struct (ls, ld) = visitMap2 x l.Left r
                                struct (
                                    HashSetInner.Create(l.Prefix, l.Mask, ls, l.Right),
                                    ld
                                )
                            elif rl = 1u then
                                let struct (rs, rd) = visitMap2 x l.Right r
                                struct (
                                    HashSetInner.Create(l.Prefix, l.Mask, l.Left, rs),
                                    rd
                                )
                            else
                                let struct (rs, rd) = r.ChooseSV2 onlyDelta
                                struct (
                                    HashSetInner.Join(l.Prefix, l, r.Prefix, rs),
                                    rd
                                )
                                
            }



[<Struct>]
type HashSetOkasaki<'T> internal(cmp: EqualityComparer<'T>, root: HashSetNode<'T>) =
    
    static member Empty = HashSetOkasaki<'T>(EqualityComparer<'T>.Default, HashSetEmpty.Instance)

    member x.Count = root.Count
    member x.IsEmpty = root.IsEmpty

    member internal x.Root = root
    
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Single(value : 'T) =  
        let cmp = EqualityComparer<'T>.Default
        HashSetOkasaki(cmp, HashSetNoCollisionLeaf.New(uint32 (cmp.GetHashCode value), value))
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfSeq(elements: seq<'T>) =  
        let cmp = EqualityComparer<'T>.Default
        let mutable r = HashMapOkasakiImplementation.HashSetEmpty.Instance 
        for v in elements do
            let hash = cmp.GetHashCode v |> uint32
            r <- r.AddInPlaceUnsafe(cmp, hash, v)
        HashSetOkasaki<'T>(cmp, r)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfList(elements: list<'T>) =  
        let cmp = EqualityComparer<'T>.Default
        let mutable r = HashMapOkasakiImplementation.HashSetEmpty.Instance 
        for v in elements do
            let hash = cmp.GetHashCode v |> uint32
            r <- r.AddInPlaceUnsafe(cmp, hash, v)
        HashSetOkasaki<'T>(cmp, r)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfArray(elements: array<'T>) =  
        let cmp = EqualityComparer<'T>.Default
        let mutable r = HashMapOkasakiImplementation.HashSetEmpty.Instance 
        for v in elements do
            let hash = cmp.GetHashCode v |> uint32
            r <- r.AddInPlaceUnsafe(cmp, hash, v)
        HashSetOkasaki<'T>(cmp, r)
        
    member inline x.ToSeq() =
        x :> seq<_>
    
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
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToArray() =
        let arr = Array.zeroCreate root.Count
        let index = ref 0
        root.CopyTo(arr, index)
        arr

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Add(value: 'T) =
        let hash = cmp.GetHashCode value |> uint32
        let newRoot = root.Add(cmp, hash, value)
        HashSetOkasaki(cmp, newRoot)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Remove(value: 'T) =
        let hash = cmp.GetHashCode value |> uint32
        let newRoot = root.Remove(cmp, hash, value)
        HashSetOkasaki(cmp, newRoot)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.TryRemove(value: 'T) =
        let hash = cmp.GetHashCode value |> uint32
        match root.TryRemove(cmp, hash, value) with
        | ValueSome newRoot -> Some (HashSetOkasaki(cmp, newRoot))
        | ValueNone -> None
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Contains(value: 'T) =
        let hash = cmp.GetHashCode value |> uint32
        root.Contains(cmp, hash, value)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Alter(value: 'T, update: bool -> bool) =
        let hash = cmp.GetHashCode value |> uint32
        let newRoot = root.Alter(cmp, hash, value, update)
        HashSetOkasaki(cmp, newRoot)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Map(mapping: 'T -> 'R) =
        let cmp = EqualityComparer<'R>.Default
        let mutable res = HashSetEmpty.Instance
        for o in x do
            let v = mapping o
            let hash = cmp.GetHashCode v
            res <- res.AddInPlaceUnsafe(cmp, uint32 hash, v)
        HashSetOkasaki(cmp, res)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Choose(mapping: 'T -> option<'R>) =
        let cmp = EqualityComparer<'R>.Default
        let mutable res = HashSetEmpty.Instance
        for o in x do
            match mapping o with
            | Some v -> 
                let hash = cmp.GetHashCode v
                res <- res.AddInPlaceUnsafe(cmp, uint32 hash, v)
            | None ->
                ()
        HashSetOkasaki(cmp, res)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ChooseV(mapping: 'T -> voption<'R>) =
        let cmp = EqualityComparer<'R>.Default
        let mutable res = HashSetEmpty.Instance
        for o in x do
            match mapping o with
            | ValueSome v -> 
                let hash = cmp.GetHashCode v
                res <- res.AddInPlaceUnsafe(cmp, uint32 hash, v)
            | ValueNone ->
                ()
        HashSetOkasaki(cmp, res)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Filter(predicate: 'T -> bool) =
        let newRoot = root.Filter(predicate)
        HashSetOkasaki(cmp, newRoot)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Iter(action: 'T -> unit) =
        root.Iter(action)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Fold(acc: 'S -> 'T -> 'S, seed : 'S) =
        let acc = OptimizedClosures.FSharpFunc<'S, 'T, 'S>.Adapt acc
        root.Fold(acc, seed)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Exists(predicate: 'T -> bool) =
        root.Exists predicate
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Forall(predicate: 'T -> bool) =
        root.Forall predicate
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ComputeDelta(l : HashSetOkasaki<'T>, r : HashSetOkasaki<'T>, add : 'T -> 'OP, remove : 'T -> 'OP) =   
        let cmp = EqualityComparer<'T>.Default
        let result = HashSetNode.computeDelta cmp add remove l.Root r.Root
        HashMapOkasaki(cmp, result)
 
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ApplyDelta(state : HashSetOkasaki<'T>, delta : HashMapOkasaki<'T, 'D>, apply : 'T -> bool -> 'D -> struct(bool * voption<'D>)) =   
        let cmp = EqualityComparer<'T>.Default
        let struct(ns, nd) = HashSetNode.applyDelta cmp apply state.Root delta.Root
        HashSetOkasaki(cmp, ns), HashMapOkasaki(cmp, nd)
     
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Union(l : HashSetOkasaki<'T>, r : HashSetOkasaki<'T>) =   
        let cmp = EqualityComparer<'T>.Default
        let result = HashSetNode.union cmp l.Root r.Root
        HashSetOkasaki(cmp, result)
 
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Difference(l : HashSetOkasaki<'T>, r : HashSetOkasaki<'T>) =   
        let cmp = EqualityComparer<'T>.Default
        let result = HashSetNode.difference cmp l.Root r.Root
        HashSetOkasaki(cmp, result)

    interface System.Collections.IEnumerable with 
        member x.GetEnumerator() = new HashSetOkasakiEnumerator<_>(root) :> _
        
    interface System.Collections.Generic.IEnumerable<'T> with 
        member x.GetEnumerator() = new HashSetOkasakiEnumerator<_>(root) :> _

and internal HashSetOkasakiEnumerator<'T>(root: HashSetNode<'T>) =
    let mutable stack = [root]
    let mutable linked: HashSetLinked<'T> = null
    let mutable current = Unchecked.defaultof<'T>

    member x.MoveNext() =
        if isNull linked then
            match stack with
            | (:? HashSetEmpty<'T>) :: rest ->
                stack <- rest 
                x.MoveNext()
            | (:? HashSetNoCollisionLeaf<'T> as l) :: rest ->
                stack <- rest
                current <- l.Value
                true
            | (:? HashSetCollisionLeaf<'T> as l) :: rest -> 
                stack <- rest
                current <- l.Value
                linked <- l.Next
                true
            | (:? HashSetInner<'T> as n) :: rest ->
                stack <- n.Left:: n.Right:: rest
                x.MoveNext()
            | _ ->
                false
        else
            current <- linked.Value
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
        
    interface System.Collections.Generic.IEnumerator<'T> with
        member x.Dispose() = x.Dispose()
        member x.Current = x.Current

[<Struct>]
type HashMapOkasaki<'K, 'V> internal(cmp: EqualityComparer<'K>, root: HashMapNode<'K, 'V>) =

    static member Empty = HashMapOkasaki<'K, 'V>(EqualityComparer<'K>.Default, HashMapEmpty.Instance)

    member x.Count = root.Count
    member x.IsEmpty = root.IsEmpty

    member internal x.Root : HashMapNode<'K, 'V> = root
    
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Single(key: 'K, value : 'V) =  
        let cmp = EqualityComparer<'K>.Default
        HashMapOkasaki(cmp, HashMapNoCollisionLeaf.New(uint32 (cmp.GetHashCode key), key, value))
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfSeq(elements: seq<'K * 'V>) =  
        let cmp = EqualityComparer<'K>.Default
        let mutable r = HashMapOkasakiImplementation.HashMapEmpty.Instance 
        for (k, v) in elements do
            let hash = cmp.GetHashCode k |> uint32
            r <- r.AddInPlaceUnsafe(cmp, hash, k, v)
        HashMapOkasaki<'K, 'V>(cmp, r)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfList(elements: list<'K * 'V>) =  
        let cmp = EqualityComparer<'K>.Default
        let mutable r = HashMapOkasakiImplementation.HashMapEmpty.Instance 
        for (k, v) in elements do
            let hash = cmp.GetHashCode k |> uint32
            r <- r.AddInPlaceUnsafe(cmp, hash, k, v)
        HashMapOkasaki<'K, 'V>(cmp, r)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfListUnoptimized(elements: list<'K * 'V>) =  
        let cmp = EqualityComparer<'K>.Default
        let mutable r = HashMapOkasakiImplementation.HashMapEmpty.Instance 
        for (k, v) in elements do
            let hash = cmp.GetHashCode k |> uint32
            r <- r.Add(cmp, hash, k, v)
        HashMapOkasaki<'K, 'V>(cmp, r)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfArray(elements: array<'K * 'V>) =  
        let cmp = EqualityComparer<'K>.Default
        let mutable r = HashMapOkasakiImplementation.HashMapEmpty.Instance 
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
    member x.ToList() =
        let arr = Array.zeroCreate root.Count
        let index = ref 0
        root.CopyTo(arr, index)
        let mutable res = []
        for i in 1 .. arr.Length do
            let i = arr.Length - i
            res <- arr.[i] :: res
        res

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToArray() =
        let arr = Array.zeroCreate root.Count
        let index = ref 0
        root.CopyTo(arr, index)
        arr
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.GetKeys() = 
        HashSetOkasaki(cmp, root.GetKeys())

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ComputeDelta(l : HashMapOkasaki<'K, 'V>, r : HashMapOkasaki<'K, 'V>, add : 'K -> 'V -> 'OP, update : 'K -> 'V -> 'V -> ValueOption<'OP>, remove : 'K -> 'V -> 'OP) =   
        let cmp = EqualityComparer<'K>.Default
        let result = HashMapNode.computeDelta cmp add update remove l.Root r.Root
        HashMapOkasaki(cmp, result)
 
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member UnionWith(l : HashMapOkasaki<'K, 'V>, r : HashMapOkasaki<'K, 'V>, resolve : 'K -> 'V -> 'V -> 'V) =   
        let cmp = EqualityComparer<'K>.Default
        let result = HashMapNode.unionWith cmp resolve l.Root r.Root
        HashMapOkasaki(cmp, result)
  
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Union(l : HashMapOkasaki<'K, 'V>, r : HashMapOkasaki<'K, 'V>) =   
        let cmp = EqualityComparer<'K>.Default
        let result = HashMapNode.union cmp l.Root r.Root
        HashMapOkasaki(cmp, result)
  
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ApplyDelta(state : HashMapOkasaki<'K, 'V>, delta : HashMapOkasaki<'K, 'D>, apply : 'K -> voption<'V> -> 'D -> struct(voption<'V> * voption<'D>)) =   
        let cmp = EqualityComparer<'K>.Default
        let struct (ns, nd) = HashMapNode.applyDelta cmp apply state.Root delta.Root
        HashMapOkasaki(cmp, ns), HashMapOkasaki(cmp, nd)


    interface System.Collections.IEnumerable with 
        member x.GetEnumerator() = new HashMapOkasakiEnumerator<_,_>(root) :> _
        
    interface System.Collections.Generic.IEnumerable<'K * 'V> with 
        member x.GetEnumerator() = new HashMapOkasakiEnumerator<_,_>(root) :> _

and internal HashMapOkasakiEnumerator<'K, 'V>(root: HashMapNode<'K, 'V>) =
    let mutable stack = [root]
    let mutable linked: HashMapLinked<'K, 'V> = null
    let mutable current = Unchecked.defaultof<'K * 'V>

    member x.MoveNext() =
        if isNull linked then
            match stack with
            | (:? HashMapEmpty<'K, 'V>) :: rest ->
                stack <- rest 
                x.MoveNext()
            | (:? HashMapNoCollisionLeaf<'K, 'V> as l) :: rest ->
                stack <- rest
                current <- l.Key, l.Value
                true
            | (:? HashMapCollisionLeaf<'K, 'V> as l) :: rest -> 
                stack <- rest
                current <- l.Key, l.Value
                linked <- l.Next
                true
            | (:? HashMapInner<'K, 'V> as n) :: rest ->
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


module HashSetOkasaki =

    /// The empty set.
    [<GeneralizableValue>]
    let empty<'T> = HashSetOkasaki<'T>.Empty

    /// The number of elements in the set `O(1)`
    let inline count (set: HashSetOkasaki<'T>) = set.Count
    
    /// Is the map empty? `O(1)`
    let inline isEmpty (set: HashSetOkasaki<'T>) = set.IsEmpty

    /// Creates a set with a single entry.
    /// `O(1)`
    let inline single (value: 'T) =
        HashSetOkasaki<'T>.Single(value)

    /// Creates a set with all entries from the seq.
    /// `O(N * log N)`
    let inline ofSeq (seq: seq<'T>) =
        HashSetOkasaki<'T>.OfSeq seq

    /// Creates a set with all entries from the Set.
    /// `O(N * log N)`
    let inline ofSet (set: Set<'T>) = 
        set |> ofSeq

    /// Creates a set with all entries from the list.
    /// `O(N * log N)`
    let inline ofList (list: list<'T>) = 
        HashSetOkasaki<'T>.OfList list

    /// Creates a set with all entries from the array.
    /// `O(N * log N)`
    let inline ofArray (arr: array<'T>) = 
        HashSetOkasaki<'T>.OfArray arr

    /// Creates a seq holding all values.
    /// `O(N)`
    let inline toSeq (set: HashSetOkasaki<'T>) = 
        set.ToSeq()

    /// Creates a list holding all values.
    /// `O(N)`
    let inline toList (set: HashSetOkasaki<'T>) = 
        set.ToList()

    /// Creates an array holding all values.
    /// `O(N)`
    let inline toArray (set: HashSetOkasaki<'T>) = 
        set.ToArray()

    /// Creates a Set holding all entries contained in the HashSet.
    /// `O(N)`
    let inline toSet (set: HashSetOkasaki<'T>) =
        set.ToSeq() |> Set.ofSeq

    /// Adds the given value. `O(log N)`
    let inline add (value: 'T) (set: HashSetOkasaki<'T>) =
        set.Add(value)

    /// Removes the given value. `O(log N)`
    let inline remove (value: 'T) (set: HashSetOkasaki<'T>) =
        set.Remove(value)
 
    /// Tries to remove the given value from the set and returns the rest of the set.
    /// `O(log N)`       
    let inline tryRemove (value: 'T) (set: HashSetOkasaki<'T>) =
        set.TryRemove(value)


    /// Tests if an entry for the given key exists. `O(log N)`
    let inline contains (value: 'T) (set: HashSetOkasaki<'T>) =
        set.Contains(value)

    let inline alter (value: 'T) (update: bool -> bool) (set: HashSetOkasaki<'T>) =
        set.Alter(value, update)
    
    /// Creates a new map (with the same keys) by applying the given function to all entries.
    /// `O(N)`
    let inline map (mapping: 'T -> 'R) (set: HashSetOkasaki<'T>) =
        set.Map mapping
    
    /// Creates a new map (with the same keys) by applying the given function to all entries.
    /// `O(N)`
    let inline choose (mapping: 'T -> option<'R>) (set: HashSetOkasaki<'T>) =
        set.Choose mapping
    
    /// Creates a new map (with the same keys) that contains all entries for which predicate was true.
    /// `O(N)`
    let inline filter (predicate: 'T -> bool) (set: HashSetOkasaki<'T>) =
        set.Filter predicate

    /// Applies the iter function to all entries of the map.
    /// `O(N)`
    let inline iter (iter: 'T -> unit) (set: HashSetOkasaki<'T>) =
        set.Iter iter

    /// Folds over all entries of the map.
    /// Note that the order for elements is undefined.
    /// `O(N)`
    let inline fold (folder: 'State -> 'T -> 'State) (seed: 'State) (set: HashSetOkasaki<'T>) =
        set.Fold(folder, seed)
        
    /// Tests whether an entry making the predicate true exists.
    /// `O(N)`
    let inline exists (predicate: 'T -> bool) (set: HashSetOkasaki<'T>) =
        set.Exists(predicate)

    /// Tests whether all entries fulfil the given predicate.
    /// `O(N)`
    let inline forall (predicate: 'T -> bool) (set: HashSetOkasaki<'T>) =
        set.Forall(predicate)

    /// Creates a new map containing all elements from l and r.
    /// Colliding entries are taken from r.
    /// `O(N + M)`        
    let inline union (l : HashSetOkasaki<'T>) (r : HashSetOkasaki<'T>) =
        HashSetOkasaki<'T>.Union(l, r)



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


    let inline computeDelta (l : HashSetOkasaki<'T>) (r : HashSetOkasaki<'T>) =
        let inline add _v = 1
        let inline remove _v = -1

        HashSetOkasaki<'T>.ComputeDelta(l, r, add, remove)

    let inline applyDelta (l : HashSetOkasaki<'T>) (r : HashMapOkasaki<'T, int>) =
        let inline apply _ (o : bool) (n : int) =
            if n < 0 then
                if o then struct (false, ValueSome -1)
                else struct(false, ValueNone)
            elif n > 0 then
                if o then struct (true, ValueNone)
                else struct (true, ValueSome 1)
            else
                struct(o, ValueNone)

        HashSetOkasaki<'T>.ApplyDelta(l, r, apply)


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



