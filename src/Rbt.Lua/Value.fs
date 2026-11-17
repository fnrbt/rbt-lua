namespace Rbt.Lua

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Text

/// Operations on Lua strings, which are immutable byte arrays.
module LuaBytes =
    /// Strings up to this length carry a precomputed hash inside their Value.
    [<Literal>]
    let HashedMaxLength = 32
    /// Marks Value.Num bits that hold a precomputed string hash (low 32 bits).
    [<Literal>]
    let HashedFlag = 0x100000000L

    let hash (b: byte[]) =
        let mutable h = HashCode()
        h.AddBytes(ReadOnlySpan<byte>(b))
        h.ToHashCode()

    let inline equal (x: byte[]) (y: byte[]) =
        obj.ReferenceEquals(x, y) || (x.Length = y.Length && ReadOnlySpan<byte>(x).SequenceEqual(ReadOnlySpan<byte>(y)))

    /// Byte-wise ordering (C Lua's strcmp/strcoll in the C locale).
    let compare (x: byte[]) (y: byte[]) = ReadOnlySpan<byte>(x).SequenceCompareTo(ReadOnlySpan<byte>(y))

    let ofString (s: string) : byte[] = Encoding.UTF8.GetBytes s
    /// Bytes of ASCII text (number formatting, type names).
    let ofAscii (s: string) : byte[] = Encoding.ASCII.GetBytes s
    /// Decode for the host: invalid UTF-8 becomes U+FFFD.
    let toString (b: byte[]) : string = Encoding.UTF8.GetString b

/// A growable byte buffer for building Lua strings.
[<Sealed>]
type ByteBuilder(capacity: int) =
    let mutable buf : byte[] = Array.zeroCreate (max 16 capacity)
    let mutable count = 0
    new() = ByteBuilder(16)
    member _.Length = count
    member private _.Ensure(extra: int) =
        if count + extra > buf.Length then
            let mutable n = buf.Length * 2
            while n < count + extra do n <- n * 2
            Array.Resize(&buf, n)
    member this.Append(b: byte) =
        this.Ensure 1
        buf.[count] <- b
        count <- count + 1
    member this.Append(bytes: byte[]) = this.Append(bytes, 0, bytes.Length)
    member this.Append(bytes: byte[], start: int, length: int) =
        this.Ensure length
        Array.blit bytes start buf count length
        count <- count + length
    /// Append ASCII text (one byte per char).
    member this.AppendAscii(s: string) =
        this.Ensure s.Length
        for i in 0 .. s.Length - 1 do buf.[count + i] <- byte s.[i]
        count <- count + s.Length
    member _.ToArray() = Array.sub buf 0 count

/// Runtime type tag. Order matters: Nil/False are the only falsy values,
/// so `byte tag > 1uy` is a branch-free truthiness test.
type Tag =
    | Nil = 0uy
    | False = 1uy
    | True = 2uy
    | Int = 3uy
    | Float = 4uy
    | Str = 5uy
    | Table = 6uy
    | Func = 7uy
    | Thread = 8uy

/// A Lua value. A 24-byte struct: numbers and booleans live inline in `Num`
/// (integers are bit-reinterpreted into the double), reference types in `Obj`.
/// This avoids the per-value heap allocation MoonSharp pays for its DynValue class.
[<Struct; CustomEquality; NoComparison>]
type Value =
    { Tag: Tag
      Num: double
      Obj: obj }

    member inline v.IsTruthy = byte v.Tag > 1uy
    member inline v.IsNil = v.Tag = Tag.Nil
    /// Read the integer payload (only valid when Tag = Int).
    member inline v.Int = BitConverter.DoubleToInt64Bits v.Num
    /// Read the float payload (only valid when Tag = Float).
    member inline v.Float = v.Num
    /// The raw bytes of a string (only valid when Tag = Str).
    member inline v.Bytes = v.Obj :?> byte[]
    /// A string's text, decoded as UTF-8 (invalid sequences become U+FFFD).
    member v.Str = LuaBytes.toString (v.Obj :?> byte[])

    /// String equality: identity, then length and (when both are known) the
    /// precomputed hashes, then the bytes.
    static member StrEq(a: Value, b: Value) =
        obj.ReferenceEquals(a.Obj, b.Obj) ||
        (let x = a.Obj :?> byte[]
         let y = b.Obj :?> byte[]
         x.Length = y.Length &&
         (let ha = BitConverter.DoubleToInt64Bits a.Num
          let hb = BitConverter.DoubleToInt64Bits b.Num
          (ha &&& hb &&& LuaBytes.HashedFlag) = 0L || ha = hb) &&
         ReadOnlySpan<byte>(x).SequenceEqual(ReadOnlySpan<byte>(y)))

    /// A float represents an integer exactly only inside the signed 64-bit range.
    static member TryExactInt64(f: double) : int64 voption =
        if f >= -9223372036854775808.0 && f < 9223372036854775808.0 && Math.Truncate f = f then
            ValueSome(int64 f)
        else ValueNone

    static member Eq(a: Value, b: Value) =
        match a.Tag, b.Tag with
        | Tag.Nil, Tag.Nil -> true
        | Tag.True, Tag.True | Tag.False, Tag.False -> true
        | Tag.Int, Tag.Int -> a.Int = b.Int
        | Tag.Float, Tag.Float -> a.Num = b.Num
        // Converting the integer to double would round large integers together.
        | Tag.Int, Tag.Float ->
            match Value.TryExactInt64 b.Num with ValueSome i -> a.Int = i | ValueNone -> false
        | Tag.Float, Tag.Int ->
            match Value.TryExactInt64 a.Num with ValueSome i -> i = b.Int | ValueNone -> false
        | Tag.Str, Tag.Str -> Value.StrEq(a, b)
        | _ when byte a.Tag = byte b.Tag -> obj.ReferenceEquals(a.Obj, b.Obj)
        | _ -> false

    override this.Equals(o: obj) =
        match o with
        | :? Value as v -> Value.Eq(this, v)
        | _ -> false

    override this.GetHashCode() =
        match this.Tag with
        | Tag.Nil -> 0
        | Tag.False -> 1
        | Tag.True -> 2
        | Tag.Int -> this.Int.GetHashCode()
        | Tag.Float ->
            match Value.TryExactInt64 this.Num with
            | ValueSome i -> i.GetHashCode()
            | ValueNone -> this.Num.GetHashCode()
        | Tag.Str ->
            let bits = BitConverter.DoubleToInt64Bits this.Num
            if bits &&& LuaBytes.HashedFlag <> 0L then int bits else LuaBytes.hash (this.Obj :?> byte[])
        | _ -> RuntimeHelpers.GetHashCode this.Obj

    interface IEquatable<Value> with
        member this.Equals(v) = Value.Eq(this, v)

/// Factory functions and constants. Coexists with the `Value` type via the
/// ModuleSuffix trick (same pattern as `Option` the type vs `Option` the module).
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Value =
    let Nil = { Tag = Tag.Nil; Num = 0.0; Obj = null }
    let True = { Tag = Tag.True; Num = 0.0; Obj = null }
    let False = { Tag = Tag.False; Num = 0.0; Obj = null }

    let inline ofBool b = if b then True else False
    let inline int (i: int64) = { Tag = Tag.Int; Num = BitConverter.Int64BitsToDouble i; Obj = null }
    let inline float (f: double) = { Tag = Tag.Float; Num = f; Obj = null }
    /// A Lua string from raw bytes (the array must not be mutated afterwards).
    let bytes (b: byte[]) =
        let num =
            if b.Length <= LuaBytes.HashedMaxLength then
                BitConverter.Int64BitsToDouble(LuaBytes.HashedFlag ||| int64 (uint32 (LuaBytes.hash b)))
            else 0.0
        { Tag = Tag.Str; Num = num; Obj = b }
    /// A Lua string holding the UTF-8 encoding of host text.
    let str (s: string) = bytes (LuaBytes.ofString s)
    let inline obj (tag: Tag) (o: obj) = { Tag = tag; Num = 0.0; Obj = o }

    /// Lua type name as returned by the `type()` builtin.
    let typeName (v: Value) =
        match v.Tag with
        | Tag.Nil -> "nil"
        | Tag.False | Tag.True -> "boolean"
        | Tag.Int | Tag.Float -> "number"
        | Tag.Str -> "string"
        | Tag.Table -> "table"
        | Tag.Func -> "function"
        | Tag.Thread -> "thread"
        | _ -> "userdata"

// ---------------------------------------------------------------------------

/// One hash-part slot: key and value stored inline. `Next` chains slots that
/// share a bucket (index into the node array, -1 = end).
[<Struct; NoEquality; NoComparison>]
type internal Node =
    val mutable Key: Value
    val mutable Val: Value
    val mutable Hash: int
    val mutable Next: int

/// A Lua table: a dense array part (keys 1..n) plus a hash part, and an
/// optional metatable.
///
/// The hash part is a compact node array in insertion order with an `int[]`
/// bucket index (open hashing by chains through `Node.Next`). A small table is
/// three allocations and lookups never touch a per-key object. Removing a key
/// leaves a dead node (nil value, key kept) until the next resize, so `next`
/// can continue from a key cleared during traversal without any traversal state.
and [<AllowNullLiteral>] LuaTable() =
    // Array part holds keys 1..acount at indices 0..acount-1.
    let mutable arr : Value[] = Array.empty
    let mutable acount = 0
    // Highest array count reached; `next` accepts integer keys up to here after
    // the array part shrinks during traversal.
    let mutable arrayHighWater = 0
    let mutable nodes : Node[] = null     // used slots: [0, used)
    let mutable buckets : int[] = null    // node index + 1; 0 = empty
    let mutable used = 0
    let mutable live = 0
    // Bucket count is odd (capacity + 1) and reduced with a multiply-based
    // modulus: sequential integer keys stay in sequential buckets (cache
    // friendly), while power-of-two strides still spread across buckets.
    let mutable divisor = 1u
    let mutable fastModMul = 0UL
    member val Metatable : LuaTable = null with get, set

    static member inline private KeyEq(a: Value, b: Value) =
        a.Tag = b.Tag &&
        (match a.Tag with
         | Tag.Int -> a.Int = b.Int
         | Tag.Float -> a.Num = b.Num
         | Tag.Str -> Value.StrEq(a, b)
         | _ -> obj.ReferenceEquals(a.Obj, b.Obj))

    member inline private _.Bucket(hash: int) =
        int (uint32 ((((fastModMul * uint64 (uint32 hash)) >>> 32) + 1UL) * uint64 divisor >>> 32))

    /// Index of the node holding `key` (live or dead), or -1.
    member private this.Find(key: Value) : int =
        if isNull nodes then -1
        else
            let h = key.GetHashCode()
            let mutable i = buckets.[this.Bucket h] - 1
            while i >= 0 && not (nodes.[i].Hash = h && LuaTable.KeyEq(nodes.[i].Key, key)) do
                i <- nodes.[i].Next
            i

    /// Rebuild the hash part with room for at least `minLive` keys, dropping dead nodes.
    member private this.Resize(minLive: int) =
        let mutable cap = 2
        while cap < minLive do cap <- cap * 2
        let old = nodes
        let oldUsed = used
        nodes <- Array.zeroCreate cap
        buckets <- Array.zeroCreate (cap + 1)
        divisor <- uint32 (cap + 1)
        fastModMul <- UInt64.MaxValue / uint64 divisor + 1UL
        used <- 0
        live <- 0
        if not (isNull old) then
            for i in 0 .. oldUsed - 1 do
                if not old.[i].Val.IsNil then this.Append(old.[i].Key, old.[i].Val, old.[i].Hash)

    member private this.Append(key: Value, value: Value, h: int) =
        let i = used
        let b = this.Bucket h
        nodes.[i].Key <- key
        nodes.[i].Val <- value
        nodes.[i].Hash <- h
        nodes.[i].Next <- buckets.[b] - 1
        buckets.[b] <- i + 1
        used <- i + 1
        live <- live + 1

    member private this.SetHash(key: Value, value: Value) =
        let i = this.Find key
        if i >= 0 then
            let wasLive = not nodes.[i].Val.IsNil
            nodes.[i].Val <- value
            if wasLive && value.IsNil then live <- live - 1
            elif not wasLive && not value.IsNil then live <- live + 1
        elif not value.IsNil then
            if isNull nodes || used = nodes.Length then this.Resize(live + 1)
            this.Append(key, value, key.GetHashCode())

    /// Normalize integral floats only when their exact value fits a Lua integer.
    static member inline private NormKey(k: Value) =
        if k.Tag = Tag.Float then
            match Value.TryExactInt64 k.Num with
            | ValueSome i -> Value.int i
            | ValueNone -> k
        else k

    /// Pull contiguous successors from the hash part into the array part after an append.
    member private this.Migrate() =
        if live > 0 then
            let mutable i = this.Find(Value.int (int64 acount + 1L))
            while i >= 0 && not nodes.[i].Val.IsNil do
                let v = nodes.[i].Val
                nodes.[i].Val <- Value.Nil
                live <- live - 1
                this.AppendArray v
                i <- this.Find(Value.int (int64 acount + 1L))

    member private _.AppendArray(v: Value) =
        if acount = arr.Length then
            let n = if arr.Length = 0 then 4 else arr.Length * 2
            let na = Array.zeroCreate n
            Array.blit arr 0 na 0 acount
            arr <- na
        arr.[acount] <- v
        acount <- acount + 1
        arrayHighWater <- max arrayHighWater acount

    member this.Get(key: Value) : Value =
        match key.Tag with
        | Tag.Int ->
            let i = key.Int
            if i >= 1L && i <= int64 acount then arr.[int i - 1]
            else
                let n = this.Find key
                if n < 0 then Value.Nil else nodes.[n].Val
        | Tag.Nil -> Value.Nil
        | _ ->
            let k = LuaTable.NormKey key
            if k.Tag = Tag.Int then this.Get k
            else
                let n = this.Find k
                if n < 0 then Value.Nil else nodes.[n].Val

    member this.GetInt(i: int64) =
        if i >= 1L && i <= int64 acount then arr.[int i - 1]
        else this.Get(Value.int i)

    member this.Set(key: Value, value: Value) =
        let key = LuaTable.NormKey key
        match key.Tag with
        | Tag.Int ->
            let i = key.Int
            if i >= 1L && i <= int64 acount then
                arr.[int i - 1] <- value
                // shrink the array part if a trailing element was nil'd
                if value.IsNil && int i = acount then
                    let mutable n = acount
                    while n > 0 && arr.[n - 1].IsNil do n <- n - 1
                    acount <- n
            elif i = int64 acount + 1L && not value.IsNil then
                this.AppendArray value
                this.Migrate()
            else this.SetHash(key, value)
        | Tag.Nil -> raise (LuaError(Value.str "table index is nil"))
        | Tag.Float when Double.IsNaN key.Num -> raise (LuaError(Value.str "table index is NaN"))
        | _ -> this.SetHash(key, value)

    /// The border `#t`: length of the array part (a valid Lua border).
    member _.Length = int64 acount

    /// Host-side enumeration: array part, then hash part in insertion order.
    /// Independent of Lua `next`, so a host may stop early.
    member _.Pairs() : seq<Value * Value> =
        seq {
            for i in 0 .. acount - 1 do
                if not arr.[i].IsNil then yield Value.int (int64 i + 1L), arr.[i]
            let mutable i = 0
            while not (isNull nodes) && i < used do
                if not nodes.[i].Val.IsNil then yield nodes.[i].Key, nodes.[i].Val
                i <- i + 1
        }

    /// The successor of `key` in traversal order: array part, then hash nodes.
    member this.Next(key: Value) : Value * Value =
        let key = LuaTable.NormKey key
        let mutable ai = 0
        let mutable ni = 0
        if not key.IsNil then
            if key.Tag = Tag.Int && key.Int >= 1L && key.Int <= int64 acount then ai <- int key.Int
            else
                let n = this.Find key
                if n >= 0 then
                    ai <- acount
                    ni <- n + 1
                elif key.Tag = Tag.Int && key.Int >= 1L && key.Int <= int64 arrayHighWater then
                    ai <- int key.Int
                else raise (LuaError(Value.str "invalid key to 'next'"))
        while ai < acount && arr.[ai].IsNil do ai <- ai + 1
        if ai < acount then Value.int (int64 ai + 1L), arr.[ai]
        else
            while ni < used && nodes.[ni].Val.IsNil do ni <- ni + 1
            if ni < used then nodes.[ni].Key, nodes.[ni].Val
            else Value.Nil, Value.Nil

    member _.ArrayCount = acount

/// The exception carrying a Lua error value through the host stack.
and LuaError(value: Value, ?traceback: string) =
    inherit Exception()
    member _.Value = value
    member _.Traceback = defaultArg traceback null
    override _.Message =
        match value.Tag with
        | Tag.Str -> LuaBytes.toString (value.Obj :?> byte[])
        | _ -> Value.typeName value
