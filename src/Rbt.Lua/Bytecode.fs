namespace Rbt.Lua

open System
open System.Threading.Tasks

/// A boxed local — a Lua upvalue. Multiple closures sharing a captured local
/// share one Cell, so mutations are visible across them.
[<Sealed>]
type Cell(v: Value) =
    member val Value = v with get, set

/// Register-VM opcodes.
type ROp =
    | LoadK = 0 | LoadNil = 1 | LoadTrue = 2 | LoadFalse = 3 | Move = 4
    | GetUpval = 5 | SetUpval = 6 | GetCell = 7 | SetCell = 8 | NewCell = 9
    | GetGlobal = 10 | SetGlobal = 11 | GetIndex = 12 | SetIdx = 13
    | Add = 14 | Sub = 15 | Mul = 16 | Div = 17 | IDiv = 18 | Mod = 19 | Pow = 20 | Concat = 21
    | Eq = 22 | Ne = 23 | Lt = 24 | Le = 25 | BAnd = 26 | BOr = 27 | BXor = 28 | Shl = 29 | Shr = 30
    | Neg = 31 | Not = 32 | Len = 33 | BNot = 34
    | Jmp = 35 | JmpF = 36 | JmpT = 37
    | NewTable = 38 | SetList = 39
    | Call = 40 | Ret = 41 | Ret0 = 42 | Ret1 = 43 | Self = 44 | Vararg = 45 | Closure = 46
    | ForPrep = 47 | ForLoop = 48
    | TForLoop = 49                 // generic-for step after the iterator call: R[A+3] = first result
    | AddI = 50 | SubI = 51         // R[A] = R[B] +/- immediate int C

[<Struct>]
type Instr = { Op: ROp; A: int; B: int; C: int }

/// A compiled register-VM function prototype.
type Proto =
    { Code: Instr[]
      Consts: Value[]
      Children: Proto[]
      Groups: (bool * int)[][]
      NumReg: int
      NumSlots: int
      NumCells: int
      NumParams: int
      ParamCells: bool[]
      ParamIndex: int[]
      IsVararg: bool
      /// params are all plain registers 0..n-1 → args arrive already in place (no copy)
      SimpleParams: bool
      Upvals: UpvalRef[]
      Name: string }

/// How a saved frame receives the values delivered to it.
module FrameKind =
    /// An ordinary call: values are the callee's results.
    [<Literal>]
    let Call = 0
    /// A `pcall` in progress: success prepends `true`; errors unwind to here.
    [<Literal>]
    let Pcall = 1
    /// An `xpcall` in progress; the handler sits in the frame's RetReg register.
    [<Literal>]
    let Xpcall = 2
    /// Waiting in `coroutine.resume`: values arrive with a leading `true`.
    [<Literal>]
    let Resume = 3
    /// Waiting in a `coroutine.wrap` function: values arrive as-is; errors propagate.
    [<Literal>]
    let Wrap = 4

/// Saved state of a suspended activation: the caller of the running function,
/// or a coroutine's running function when it yields or awaits.
[<Struct>]
type RFrame =
    val mutable P: Proto
    val mutable Base: int
    val mutable Ip: int
    val mutable Upvals: Cell[]
    val mutable Cells: Cell[]
    val mutable Varargs: Value[]
    /// Destination register for delivered values, relative to Base.
    val mutable RetReg: int
    /// 0 = all values (sets Top), otherwise exactly Want - 1 values.
    val mutable Want: int
    val mutable Kind: int

type CoStatus =
    | Suspended = 0
    | Running = 1
    | Normal = 2
    | Dead = 3

/// A Lua thread in Lua's sense — a coroutine — not an OS thread. All VM
/// execution state lives here (registers and saved frames), so suspending is
/// just leaving it in place and resuming is reloading the top frame. Roots
/// (the interpreter's main state, or an async host call) cannot yield.
[<Sealed; AllowNullLiteral>]
type Coroutine(stackSize: int, frameCount: int, isRoot: bool) =
    [<Literal>]
    static let MaxStack = 1000000

    member val Stack : Value[] = Array.zeroCreate stackSize with get, set
    member val Top = 0 with get, set
    member val Frames : RFrame[] = Array.zeroCreate frameCount with get, set
    member val Fp = 0 with get, set
    member val Status = (if isRoot then CoStatus.Running else CoStatus.Suspended) with get, set
    /// The coroutine that resumed this one, while it runs.
    member val Resumer : Coroutine = null with get, set
    /// The body function, until the coroutine first runs.
    member val Body = Value.Nil with get, set
    member val Started = isRoot with get, set
    /// Native (host) re-entries into the VM currently active on this coroutine.
    /// Yielding is impossible while any are active.
    member val NativeDepth = 0 with get, set
    /// The error object a coroutine died with, for `coroutine.close`.
    member val Error = Value.Nil with get, set
    member val Errored = false with get, set
    /// The host task an async root is waiting on.
    member val Pending : Task<Value[]> = null with get, set
    member _.IsRoot = isRoot

    /// Ensure the register stack holds at least `needed` slots.
    member this.Grow(needed: int) =
        if needed > this.Stack.Length then
            if needed > MaxStack then raise (LuaError(Value.str "stack overflow"))
            let mutable n = max 16 (this.Stack.Length * 2)
            while n < needed do n <- n * 2
            let ns = Array.zeroCreate (min n MaxStack)
            Array.blit this.Stack 0 ns 0 this.Stack.Length
            this.Stack <- ns

    /// Reserve one more frame slot and return its index (Fp is advanced).
    member this.PushFrameSlot() =
        let fi = this.Fp
        if fi >= this.Frames.Length then
            let nf = Array.zeroCreate (this.Frames.Length * 2 + 4)
            Array.Copy(this.Frames, nf, this.Frames.Length)
            this.Frames <- nf
        this.Fp <- fi + 1
        fi
