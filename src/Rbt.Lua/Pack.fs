namespace Rbt.Lua

open System

/// string.pack / string.unpack / string.packsize: a port of lstrlib.c's
/// binary packing, with the sizes of a 64-bit (LP64, little-endian) C Lua.
module Pack =

    type private Kind =
        | Int | Uint | Float | Number | Double | Char | String | Zstr | Padding | PadAlign | Nop

    [<Literal>]
    let private MaxIntSize = 16
    [<Literal>]
    let private SzInt = 8
    [<Literal>]
    let private NativeAlign = 8

    type private Header = { mutable Little: bool; mutable MaxAlign: int }

    let private fail (msg: string) : 'a = raise (LuaError(Value.str msg))
    let private argError fn n (msg: string) : 'a = fail (sprintf "bad argument #%d to '%s' (%s)" n fn msg)

    let private isDigit (b: byte) = b >= '0'B && b <= '9'B

    let private getNum (fmt: byte[]) (pos: byref<int>) (df: int) =
        if pos >= fmt.Length || not (isDigit fmt.[pos]) then df
        else
            let mutable a = 0
            let mutable go = true
            while go do
                a <- a * 10 + int (fmt.[pos] - '0'B)
                pos <- pos + 1
                go <- pos < fmt.Length && isDigit fmt.[pos] && a <= (Int32.MaxValue - 9) / 10
            a

    let private getNumLimit (fmt: byte[]) (pos: byref<int>) (df: int) =
        let sz = getNum fmt &pos df
        if sz > MaxIntSize || sz <= 0 then fail (sprintf "integral size (%d) out of limits [1,%d]" sz MaxIntSize)
        sz

    let private getOption (h: Header) (fmt: byte[]) (pos: byref<int>) (size: byref<int>) : Kind =
        let opt = fmt.[pos]
        pos <- pos + 1
        size <- 0
        match opt with
        | 'b'B -> size <- 1; Int
        | 'B'B -> size <- 1; Uint
        | 'h'B -> size <- 2; Int
        | 'H'B -> size <- 2; Uint
        | 'l'B | 'j'B -> size <- 8; Int
        | 'L'B | 'J'B | 'T'B -> size <- 8; Uint
        | 'f'B -> size <- 4; Float
        | 'n'B -> size <- 8; Number
        | 'd'B -> size <- 8; Double
        | 'i'B -> size <- getNumLimit fmt &pos 4; Int
        | 'I'B -> size <- getNumLimit fmt &pos 4; Uint
        | 's'B -> size <- getNumLimit fmt &pos 8; String
        | 'c'B ->
            size <- getNum fmt &pos -1
            if size = -1 then fail "missing size for format option 'c'"
            Char
        | 'z'B -> Zstr
        | 'x'B -> size <- 1; Padding
        | 'X'B -> PadAlign
        | ' 'B -> Nop
        | '<'B | '='B -> h.Little <- true; Nop
        | '>'B -> h.Little <- false; Nop
        | '!'B -> h.MaxAlign <- getNumLimit fmt &pos NativeAlign; Nop
        | c -> fail (sprintf "invalid format option '%c'" (char c))

    /// Read the next option and the padding needed to align it at `total`.
    let private getDetails (h: Header) (total: int64) (fmt: byte[]) (pos: byref<int>) (size: byref<int>) (toAlign: byref<int>) : Kind =
        let opt = getOption h fmt &pos &size
        let mutable align = size
        if opt = PadAlign then
            let mutable next = Nop
            if pos >= fmt.Length then argError "string.pack" 1 "invalid next option for option 'X'"
            next <- getOption h fmt &pos &align
            if next = Char || align = 0 then argError "string.pack" 1 "invalid next option for option 'X'"
        if align <= 1 || opt = Char then toAlign <- 0
        else
            if align > h.MaxAlign then align <- h.MaxAlign
            if (align &&& (align - 1)) <> 0 then argError "string.pack" 1 "format asks for alignment not power of 2"
            toAlign <- (align - int (total &&& int64 (align - 1))) &&& (align - 1)
        opt

    let private packInt (sb: ByteBuilder) (n: uint64) (little: bool) (size: int) (negative: bool) =
        let buf = Array.zeroCreate<byte> size
        let mutable v = n
        for i in 0 .. min size SzInt - 1 do
            buf.[if little then i else size - 1 - i] <- byte (v &&& 0xFFUL)
            v <- v >>> 8
        if negative && size > SzInt then
            for i in SzInt .. size - 1 do buf.[if little then i else size - 1 - i] <- 0xFFuy
        sb.Append buf

    let private unpackInt (s: byte[]) (at: int) (little: bool) (size: int) (signed: bool) : int64 =
        let limit = min size SzInt
        let mutable res = 0UL
        for i in limit - 1 .. -1 .. 0 do
            res <- (res <<< 8) ||| uint64 s.[at + (if little then i else size - 1 - i)]
        if size < SzInt then
            if signed then
                let mask = 1UL <<< (size * 8 - 1)
                res <- (res ^^^ mask) - mask
        elif size > SzInt then
            let mask = if not signed || int64 res >= 0L then 0uy else 0xFFuy
            for i in limit .. size - 1 do
                if s.[at + (if little then i else size - 1 - i)] <> mask then
                    fail (sprintf "%d-byte integer does not fit into Lua Integer" size)
        int64 res

    let private copyEndian (sb: ByteBuilder) (bytes: byte[]) (little: bool) =
        // .NET hosts are little-endian, like the reference build.
        if little then sb.Append bytes else sb.Append(Array.rev bytes)

    let private readEndian (s: byte[]) (at: int) (size: int) (little: bool) =
        let b = Array.sub s at size
        if little then b else Array.rev b

    let pack (interp: Interp) (fmt: byte[]) (a: Value[]) : byte[] =
        let h = { Little = true; MaxAlign = 1 }
        let sb = ByteBuilder()
        let mutable pos = 0
        let mutable total = 0L
        let mutable arg = 1   // a.[0] is the format
        let argv i = if i < a.Length then a.[i] else Value.Nil
        // C Lua's str_pack pushes a nil marker after its arguments, so the first
        // missing argument reports "got nil".
        let gotName i = if i <= a.Length then Value.typeName (argv i) else "no value"
        let checkInt i =
            match interp.ToInteger(argv i) with
            | ValueSome n -> n
            | ValueNone ->
                match interp.ToNumber(argv i) with
                | ValueSome _ -> argError "string.pack" (i + 1) "number has no integer representation"
                | ValueNone -> argError "string.pack" (i + 1) (sprintf "number expected, got %s" (gotName i))
        let checkNum i =
            match interp.ToNumber(argv i) with
            | ValueSome n -> interp.ToFloat n
            | ValueNone -> argError "string.pack" (i + 1) (sprintf "number expected, got %s" (gotName i))
        let checkStr i =
            let v = argv i
            match v.Tag with
            | Tag.Str -> v.Bytes
            | Tag.Int | Tag.Float -> Numbers.toBytes v
            | _ -> argError "string.pack" (i + 1) (sprintf "string expected, got %s" (gotName i))
        while pos < fmt.Length do
            let mutable size = 0
            let mutable toAlign = 0
            let opt = getDetails h total fmt &pos &size &toAlign
            total <- total + int64 toAlign + int64 size
            for _ in 1 .. toAlign do sb.Append 0uy
            match opt with
            | Int ->
                let n = checkInt arg
                if size < SzInt then
                    let lim = 1L <<< (size * 8 - 1)
                    if not (-lim <= n && n < lim) then argError "string.pack" (arg + 1) "integer overflow"
                packInt sb (uint64 n) h.Little size (n < 0L)
                arg <- arg + 1
            | Uint ->
                let n = checkInt arg
                if size < SzInt && not (uint64 n < (1UL <<< (size * 8))) then argError "string.pack" (arg + 1) "unsigned overflow"
                packInt sb (uint64 n) h.Little size false
                arg <- arg + 1
            | Float -> copyEndian sb (BitConverter.GetBytes(float32 (checkNum arg))) h.Little; arg <- arg + 1
            | Number | Double -> copyEndian sb (BitConverter.GetBytes(checkNum arg)) h.Little; arg <- arg + 1
            | Char ->
                let s = checkStr arg
                if s.Length > size then argError "string.pack" (arg + 1) "string longer than given size"
                sb.Append s
                for _ in s.Length + 1 .. size do sb.Append 0uy
                arg <- arg + 1
            | String ->
                let s = checkStr arg
                if not (size >= 8 || uint64 s.Length < (1UL <<< (size * 8))) then
                    argError "string.pack" (arg + 1) "string length does not fit in given size"
                packInt sb (uint64 s.Length) h.Little size false
                sb.Append s
                total <- total + int64 s.Length
                arg <- arg + 1
            | Zstr ->
                let s = checkStr arg
                if Array.contains 0uy s then argError "string.pack" (arg + 1) "string contains zeros"
                sb.Append s
                sb.Append 0uy
                total <- total + int64 s.Length + 1L
                arg <- arg + 1
            | Padding -> sb.Append 0uy
            | PadAlign | Nop -> ()
        sb.ToArray()

    let packsize (fmt: byte[]) : int64 =
        let h = { Little = true; MaxAlign = 1 }
        let mutable pos = 0
        let mutable total = 0L
        while pos < fmt.Length do
            let mutable size = 0
            let mutable toAlign = 0
            let opt = getDetails h total fmt &pos &size &toAlign
            if opt = String || opt = Zstr then argError "string.packsize" 1 "variable-length format"
            let size = int64 size + int64 toAlign
            if total > int64 Int32.MaxValue - size then argError "string.packsize" 1 "format result too large"
            total <- total + size
        total

    let unpack (fmt: byte[]) (data: byte[]) (init: int64) : Value[] =
        let h = { Little = true; MaxAlign = 1 }
        let ld = int64 data.Length
        let start = if init > 0L then init elif init = 0L then 1L elif init < -ld then 1L else ld + init + 1L
        let mutable pos = start - 1L
        if pos > ld then argError "string.unpack" 3 "initial position out of string"
        let out = ResizeArray<Value>()
        let mutable fp = 0
        while fp < fmt.Length do
            let mutable size = 0
            let mutable toAlign = 0
            let opt = getDetails h pos fmt &fp &size &toAlign
            if int64 toAlign + int64 size > ld - pos then argError "string.unpack" 2 "data string too short"
            pos <- pos + int64 toAlign
            let at = int pos
            match opt with
            | Int | Uint -> out.Add(Value.int (unpackInt data at h.Little size (opt = Int)))
            | Float -> out.Add(Value.float (float (BitConverter.ToSingle(readEndian data at 4 h.Little, 0))))
            | Number | Double -> out.Add(Value.float (BitConverter.ToDouble(readEndian data at 8 h.Little, 0)))
            | Char -> out.Add(Value.bytes (Array.sub data at size))
            | String ->
                let len = unpackInt data at h.Little size false
                if uint64 len > uint64 (ld - pos - int64 size) then argError "string.unpack" 2 "data string too short"
                out.Add(Value.bytes (Array.sub data (at + size) (int len)))
                pos <- pos + len
            | Zstr ->
                let mutable e = at
                while e < data.Length && data.[e] <> 0uy do e <- e + 1
                if e >= data.Length then argError "string.unpack" 2 "unfinished string for format 'z'"
                out.Add(Value.bytes (Array.sub data at (e - at)))
                pos <- pos + int64 (e - at) + 1L
            | Padding | PadAlign | Nop -> ()
            pos <- pos + int64 size
        out.Add(Value.int (pos + 1L))
        out.ToArray()
