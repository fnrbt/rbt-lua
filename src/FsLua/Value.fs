namespace FsLua

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

    static member Eq(a: Value, b: Value) =
        match a.Tag, b.Tag with
        | Tag.Nil, Tag.Nil -> true
        | Tag.True, Tag.True | Tag.False, Tag.False -> true
        | Tag.Int, Tag.Int -> a.Int = b.Int
        | Tag.Float, Tag.Float -> a.Num = b.Num
        // cross-subtype numeric equality: 3 == 3.0
        | Tag.Int, Tag.Float -> double a.Int = b.Num
        | Tag.Float, Tag.Int -> a.Num = double b.Int
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
        | Tag.Float -> this.Num.GetHashCode()
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

/// A Lua table: a hybrid of a dense array part (1-based contiguous integer keys)
/// and a hash part for everything else, plus an optional metatable.
[<AllowNullLiteral>]
type LuaTable() =
    // Array part holds keys 1..Count at indices 0..Count-1.
    let mutable arr : Value[] = Array.empty
    let mutable acount = 0
    let mutable hash : Dictionary<Value, Value> = null
    member val Metatable : LuaTable = null with get, set

    member private _.EnsureHash() =
        if isNull hash then hash <- Dictionary<Value, Value>()
        hash

    /// Normalize a float key with an integral value to an integer key (Lua semantics).
    static member inline private NormKey(k: Value) =
        if k.Tag = Tag.Float then
            let f = k.Num
            let i = int64 f
            if double i = f && not (Double.IsInfinity f) then Value.int i else k
        else k

    /// Pull contiguous successors from the hash part into the array part after an append.
    member private this.Migrate() =
        if not (isNull hash) then
            let mutable next = Value.int (int64 acount + 1L)
            let mutable v = Unchecked.defaultof<Value>
            while hash.TryGetValue(next, &v) do
                hash.Remove next |> ignore
                this.Append v
                next <- Value.int (int64 acount + 1L)

    member private _.Append(v: Value) =
        if acount = arr.Length then
            let n = if arr.Length = 0 then 4 else arr.Length * 2
            let na = Array.zeroCreate n
            Array.blit arr 0 na 0 acount
            arr <- na
        arr.[acount] <- v
        acount <- acount + 1

    member this.Get(key: Value) : Value =
        match key.Tag with
        | Tag.Int ->
            let i = key.Int
            if i >= 1L && i <= int64 acount then arr.[int i - 1]
            elif isNull hash then Value.Nil
            else match hash.TryGetValue key with | true, v -> v | _ -> Value.Nil
        | Tag.Nil -> Value.Nil
        | _ ->
            let k = LuaTable.NormKey key
            if k.Tag = Tag.Int then this.Get k
            elif isNull hash then Value.Nil
            else match hash.TryGetValue k with | true, v -> v | _ -> Value.Nil

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
            elif value.IsNil then
                if not (isNull hash) then hash.Remove key |> ignore
            else (this.EnsureHash()).[key] <- value
        | Tag.Nil -> raise (LuaError(Value.str "table index is nil"))
        | Tag.Float when Double.IsNaN key.Num -> raise (LuaError(Value.str "table index is NaN"))
        | _ ->
            if value.IsNil then (if not (isNull hash) then hash.Remove key |> ignore)
            else (this.EnsureHash()).[key] <- value

    /// The border `#t`: length of the array part (a valid Lua border).
    member _.Length = int64 acount

    /// Enumerate for `next`/`pairs`. Yields array part then hash part.
    member _.Pairs() : seq<Value * Value> =
        seq {
            for i in 0 .. acount - 1 do
                if not arr.[i].IsNil then yield Value.int (int64 i + 1L), arr.[i]
            if not (isNull hash) then
                for kv in hash do yield kv.Key, kv.Value
        }

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
