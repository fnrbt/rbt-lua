namespace Rbt.Lua

open System
open System.Collections.Generic
open System.Runtime.CompilerServices

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
    member inline v.Str = v.Obj :?> string

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
        | Tag.Str, Tag.Str -> String.Equals(a.Obj :?> string, b.Obj :?> string)
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
        | Tag.Str -> (this.Obj :?> string).GetHashCode()
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
    let inline str (s: string) = { Tag = Tag.Str; Num = 0.0; Obj = s }
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
        | _ -> "userdata"

// ---------------------------------------------------------------------------

[<AllowNullLiteral>]
type private HashEntry(key: Value, value: Value) =
    member _.Key = key
    member val Value = value with get, set
    member val Next : HashEntry = null with get, set
    member val Prev : HashEntry = null with get, set

/// A Lua table: a hybrid of a dense array part (1-based contiguous integer keys)
/// and a hash part for everything else, plus an optional metatable.
[<AllowNullLiteral>]
type LuaTable() =
    // Array part holds keys 1..Count at indices 0..Count-1.
    let mutable arr : Value[] = Array.empty
    let mutable acount = 0
    let mutable hash : Dictionary<Value, HashEntry> = null
    let mutable first : HashEntry = null
    let mutable last : HashEntry = null
    let mutable arrayHighWater = 0
    let mutable traversing = false
    let mutable removed : ResizeArray<HashEntry> = null
    member val Metatable : LuaTable = null with get, set

    member private _.EnsureHash() =
        if isNull hash then hash <- Dictionary<Value, HashEntry>()
        hash

    member private _.Unlink(entry: HashEntry) =
        if isNull entry.Prev then first <- entry.Next else entry.Prev.Next <- entry.Next
        if isNull entry.Next then last <- entry.Prev else entry.Next.Prev <- entry.Prev

    member private _.MarkRemoved(entry: HashEntry) =
        if not entry.Value.IsNil then
            entry.Value <- Value.Nil
            if isNull removed then removed <- ResizeArray()
            removed.Add entry

    member private this.ClearRemoved() =
        if not (isNull removed) then
            for entry in removed do
                if entry.Value.IsNil then
                    let mutable current = Unchecked.defaultof<HashEntry>
                    if hash.TryGetValue(entry.Key, &current) && obj.ReferenceEquals(entry, current) then
                        hash.Remove entry.Key |> ignore
                        this.Unlink entry
            removed.Clear()

    member private this.SetHash(key: Value, value: Value) =
        if not (isNull hash) && hash.ContainsKey key then
            let entry = hash.[key]
            if value.IsNil && not traversing then
                hash.Remove key |> ignore
                this.Unlink entry
            else if value.IsNil then this.MarkRemoved entry else entry.Value <- value
        elif not value.IsNil then
            let entry = HashEntry(key, value)
            if isNull last then first <- entry else (last.Next <- entry; entry.Prev <- last)
            last <- entry
            (this.EnsureHash()).Add(key, entry)

    /// Normalize integral floats only when their exact value fits a Lua integer.
    static member inline private NormKey(k: Value) =
        if k.Tag = Tag.Float then
            match Value.TryExactInt64 k.Num with
            | ValueSome i -> Value.int i
            | ValueNone -> k
        else k

    /// Pull contiguous successors from the hash part into the array part after an append.
    member private this.Migrate() =
        if not (isNull hash) then
            let mutable next = Value.int (int64 acount + 1L)
            let mutable entry = Unchecked.defaultof<HashEntry>
            while hash.TryGetValue(next, &entry) && not entry.Value.IsNil do
                let value = entry.Value
                if traversing then this.MarkRemoved entry
                else
                    hash.Remove next |> ignore
                    this.Unlink entry
                this.Append value
                next <- Value.int (int64 acount + 1L)

    member private _.Append(v: Value) =
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
            elif isNull hash then Value.Nil
            else match hash.TryGetValue key with | true, entry -> entry.Value | _ -> Value.Nil
        | Tag.Nil -> Value.Nil
        | _ ->
            let k = LuaTable.NormKey key
            if k.Tag = Tag.Int then this.Get k
            elif isNull hash then Value.Nil
            else match hash.TryGetValue k with | true, entry -> entry.Value | _ -> Value.Nil

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
                this.Append value
                this.Migrate()
            else this.SetHash(key, value)
        | Tag.Nil -> raise (LuaError(Value.str "table index is nil"))
        | Tag.Float when Double.IsNaN key.Num -> raise (LuaError(Value.str "table index is NaN"))
        | _ ->
            this.SetHash(key, value)

    /// The border `#t`: length of the array part (a valid Lua border).
    member _.Length = int64 acount

    /// Host-side enumeration: array part, then hash part in insertion order.
    /// Unlike `Next`, it does not touch Lua traversal state, so a host may stop
    /// early without affecting a Lua `next`/`pairs` loop. Entries cleared during
    /// a Lua traversal are skipped.
    member _.Pairs() : seq<Value * Value> =
        seq {
            for i in 0 .. acount - 1 do
                if not arr.[i].IsNil then yield Value.int (int64 i + 1L), arr.[i]
            let mutable entry = first
            while not (isNull entry) do
                if not entry.Value.IsNil then yield entry.Key, entry.Value
                entry <- entry.Next
        }

    /// Find the successor without restarting an enumeration. Deleted hash entries
    /// remain as traversal cursors until the next pass starts.
    member this.Next(key: Value) : Value * Value =
        let key = LuaTable.NormKey key
        if key.IsNil then
            this.ClearRemoved()
            traversing <- true
        let mutable index = -1
        let mutable entry = first
        if not key.IsNil then
            if key.Tag = Tag.Int && key.Int >= 1L && key.Int <= int64 arrayHighWater &&
               (isNull hash || not (hash.ContainsKey key) ||
                (key.Int <= int64 acount && not arr.[int key.Int - 1].IsNil)) then
                index <- int key.Int - 1
            elif not (isNull hash) then
                match hash.TryGetValue key with
                | true, found -> entry <- found.Next
                | _ -> raise (LuaError(Value.str "invalid key to 'next'"))
            else raise (LuaError(Value.str "invalid key to 'next'"))
        let mutable result = Value.Nil, Value.Nil
        if index >= 0 || key.IsNil then
            let mutable i = index + 1
            while i < acount && arr.[i].IsNil do i <- i + 1
            if i < acount then result <- Value.int (int64 i + 1L), arr.[i]
        if (fst result).IsNil then
            while not (isNull entry) && entry.Value.IsNil do entry <- entry.Next
            if not (isNull entry) then result <- entry.Key, entry.Value
            else
                traversing <- false
                this.ClearRemoved()
        result


    member _.ArrayCount = acount

/// The exception carrying a Lua error value through the host stack.
and LuaError(value: Value, ?traceback: string) =
    inherit Exception()
    member _.Value = value
    member _.Traceback = defaultArg traceback null
    override _.Message =
        match value.Tag with
        | Tag.Str -> value.Obj :?> string
        | _ -> Value.typeName value
