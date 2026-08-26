namespace Rbt.Lua

/// Unary operators.
type UnOp =
    | Neg   // -
    | Not   // not
    | Len   // #
    | BNot  // ~

/// Binary operators (logical and/or handled specially for short-circuiting).
type BinOp =
    | Add | Sub | Mul | Div | IDiv | Mod | Pow
    | Concat
    | Eq | Ne | Lt | Le | Gt | Ge
    | And | Or
    | BAnd | BOr | BXor | Shl | Shr

/// Expressions. Variable references are by name; a later pass resolves them to slots.
type Expr =
    | ENil
    | ETrue
    | EFalse
    | EInt of int64
    | EFloat of double
    | EStr of string
    | EVararg
    | EName of string
    | EIndex of Expr * Expr
    | ECall of Expr * Expr list
    | EMethod of Expr * string * Expr list      // obj:name(args)
    | EUnary of UnOp * Expr
    | EBinary of BinOp * Expr * Expr
    | EFunc of FuncBody
    | ETable of TableField list
    | EParen of Expr                            // (e) — adjusts a multi-value to exactly one

and TableField =
    | FPos of Expr                              // positional array entry
    | FNamed of string * Expr                   // name = expr
    | FKeyed of Expr * Expr                     // [key] = expr

and FuncBody =
    { Pars: string list
      IsVararg: bool
      Body: Block
      /// Source name, for diagnostics.
      Name: string }

and Stat =
    | SLocal of string list * Expr list
    | SAssign of Expr list * Expr list
    | SCall of Expr
    | SDo of Block
    | SWhile of Expr * Block
    | SRepeat of Block * Expr
    | SIf of (Expr * Block) list * Block option
    | SNumFor of string * Expr * Expr * Expr option * Block
    | SGenFor of string list * Expr list * Block
    | SLocalFunc of string * FuncBody
    | SReturn of Expr list
    | SBreak
    | SGoto of string
    | SLabel of string

and Block = Stat list
