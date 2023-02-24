include "common.mc"
include "name.mc"
include "string.mc"

lang RTPPL
  syn Top =
  syn Stmt =
  syn Expr =
  syn Type =
  syn Const =

  syn ChannelSpec =
  | ExtChannel Name
  | LocalChannel {task : Name, id : Name}

  type Task = {id : Name, e : Expr}
  type Connection = {from : ChannelSpec, to : ChannelSpec}
  type Main = {params : [(Name, Type)], tasks : [Task], connections : [Connection]}
  type Program = {tops : [Top], main : Main}
end

-- Top-level definitions of the RTPPL program, which always come before the
-- main definition.
lang RTPPLTop = RTPPL
  syn Channel =
  | Input (Name, Type)
  | Output (Name, Type)

  syn Top =
  -- sensor <id> : <ty>
  | Sensor {id : Name, ty : Type}

  -- actuator <id> : <ty>
  | Actuator {id : Name, ty : Type}

  -- const <id> : <ty> = <e>
  | Constant {id : Name, ty : Type, e : Expr}

  -- def <id> (<params>) : <ty> { <body> }
  | Function {id : Name, params : [(Name, Type)], ty : Type, body : ([Channel], [Stmt])}

  -- type <id> = <ty>
  | TypeAlias {id : Name, ty : Type}
end

lang RTPPLStmt = RTPPL
  syn Stmt =
  -- let <id> : <ty> = <e>
  | Binding {id : Name, ty : Type, e : Expr}

  -- <id> = <e>
  | Reassign {id : Name, e : Expr}

  -- if <cond> { <thn> } (else if ... ) (else { <els> })
  | Condition {cond : Expr, thn : [Stmt], elsIfs : [(Expr, [Stmt])], els : Option [Stmt]}

  -- for <id> in <e> { <body> }
  | ForIn {id : Name, e : Expr, body : [Stmt]}

  -- loop { <body> }
  | Loop {body : [Stmt]}

  -- return <e>
  | Return {e : Expr}

  -- break
  | Break ()
end

lang RTPPLExpr = RTPPL
  syn Expr =
  -- NOTE: What built-in functions do we provide? E.g. for manipulating
  -- sequences/arrays or whatever we decide to call them.
  -- <id>(<args>)
  | FunctionCall {id : Name, args : [Expr]}

  -- <e>[<idx>]
  | ArrayAccess {e : Expr, idx : Expr}

  -- <lhs> <op> <rhs>
  | BinaryOp {op : BinOp, lhs : Expr, rhs : Expr}

  -- <op> <e>
  | UnaryOp {op : UnOp, e : Expr}

  -- [<elems>]
  | Seq {elems : [Expr]}

  -- Literal values, such as '2' and 'false'
  | Literal {const : Const}

  -- \<id> : <ty> -> <body>
  | Lambda {id : Name, ty : Type, body : Expr}

  -- NOTE: Should probably restrict it so that <lo>, <hi> : Int and so that
  -- <lo> must be an integer literal. Also, what would the semantics be if we
  -- have '0..n' and n is nonpositive? Empty range?
  -- <lo>..<hi>
  | Range {lo : Expr, hi : Expr}

  syn BinOp =
  | Plus ()
  | Minus ()
  | Times ()
  | Divide ()
  | Eq ()
  -- ... and so on

  syn UnOp =
  | UMinus ()
end

lang RTPPLSpecial = RTPPL
  syn Expr =
  -- sdelay(<t>) : Float -> Unit
  | Sdelay {t : Expr}

  -- assume(<d>) : Dist(T) -> T
  | Assume {d : Expr}

  -- weight(<w>) : Float -> Unit
  | Weight {w : Expr}

  -- TODO: How do we configure the inference method?
  -- infer(<model>) : (Unit -> T) -> Dist T
  | Infer {model : Expr}
end

lang RTPPLType = RTPPL
  syn Type =
  | TInt ()
  | TFloat ()
  | TBool ()
  | TUnit () -- explicit unit or just alias for empty record type?
  | TSeq {ty : Type}
  | TRecord {fields : [(String, Type)]}
  | TFunction {from : Type, to : Type}
end

lang RTPPLConst = RTPPL
  syn Const =
  | CInt {i : Int}
  | CFloat {f : Float}
  | CBool {b : Bool}
end

-- NOTE(larshum, 2023-02-24): A very simple pretty-printer which can handle a
-- sufficiently large portion of the above AST to pretty-print the simple
-- example.
lang RTPPLAst =
  RTPPLTop + RTPPLStmt + RTPPLExpr + RTPPLSpecial + RTPPLType + RTPPLConst

  sem pprintRtpplProgram : Program -> String
  sem pprintRtpplProgram =
  | prog ->
    let topsStr = strJoin "\n" (map pprintRtpplTop prog.tops) in
    let mainStr = pprintRtpplMain prog.main in
    join [topsStr, "\n", mainStr]

  sem pprintRtpplTop : Top -> String
  sem pprintRtpplTop =
  | Sensor {id = id, ty = ty} ->
    join ["sensor ", nameGetStr id, " : ", pprintRtpplType ty]
  | Actuator {id = id, ty = ty} ->
    join ["actuator ", nameGetStr id, " : ", pprintRtpplType ty]
  | Function {id = id, params = params, ty = ty, body = (channels, stmts)} ->
    let paramsStr = pprintRtpplParams params in
    let chs = strJoin "" (map (lam ch. concat (pprintRtpplChannel ch) "\n") channels) in
    let stmtStrs = strJoin "\n" (map (pprintRtpplStmt 2) stmts) in
    join ["def ", nameGetStr id, "(", paramsStr, ") : ", pprintRtpplType ty,
          " {\n", chs, stmtStrs, "}"]

  sem pprintRtpplChannel : Channel -> String
  sem pprintRtpplChannel =
  | Input (id, ty) -> join ["  input ", nameGetStr id, " : ", pprintRtpplType ty]
  | Output (id, ty) -> join ["  output ", nameGetStr id, " : ", pprintRtpplType ty]

  sem pprintRtpplParams : [(Name, Type)] -> String
  sem pprintRtpplParams =
  | [(id, ty)] ++ rest ->
    let tailstr =
      if null rest then ""
      else concat ", " (pprintRtpplParams rest)
    in
    join [nameGetStr id, " : ", pprintRtpplType ty, tailstr]
  | [] -> ""

  sem pprintRtpplMain : Main -> String
  sem pprintRtpplMain =
  | {params = params, tasks = tasks, connections = connections} ->
    let tasksStr = strJoin "\n" (map pprintRtpplTask tasks) in
    let connectionsStr = strJoin "\n" (map pprintRtpplConnection connections) in
    join ["main() {\n", tasksStr, "\n", connectionsStr, "\n}"]

  sem pprintRtpplTask : Task -> String
  sem pprintRtpplTask =
  | {id = id, e = e} ->
    join ["  task ", nameGetStr id, " = ", pprintRtpplExpr 2 e]

  sem pprintRtpplConnection : Connection -> String
  sem pprintRtpplConnection =
  | {from = from, to = to} ->
    join ["  ", pprintRtpplChannelSpec from, " -> ", pprintRtpplChannelSpec to]

  sem pprintRtpplChannelSpec : ChannelSpec -> String
  sem pprintRtpplChannelSpec =
  | ExtChannel id -> nameGetStr id
  | LocalChannel {task = task, id = id} ->
    join [nameGetStr task, ".", nameGetStr id]

  sem pprintRtpplStmt : Int -> Stmt -> String
  sem pprintRtpplStmt indent =

  sem pprintRtpplExpr : Int -> Expr -> String
  sem pprintRtpplExpr indent =
  | FunctionCall {id = id, args = args} ->
    join [nameGetStr id, "(", pprintRtpplArgs indent args, ")"]
  | Literal {const = c} -> pprintRtpplConst c

  sem pprintRtpplArgs : Int -> [Expr] -> String
  sem pprintRtpplArgs indent =
  | [arg] ++ args ->
    let tailstr =
      if null args then ""
      else concat ", " (pprintRtpplArgs indent args)
    in
    concat (pprintRtpplExpr indent arg) tailstr
  | [] -> ""

  sem pprintRtpplConst : Const -> String
  sem pprintRtpplConst =
  | CInt {i = i} -> int2string i
  | CFloat {f = f} -> float2string f
  | CBool {b = true} -> "true"
  | CBool {b = false} -> "false"

  sem pprintRtpplType : Type -> String
  sem pprintRtpplType =
  | TFloat _ -> "Float"
  | TUnit _ -> "Unit"
end

mexpr

use RTPPLAst in

let emptyProg = {tops = [], main = {params = [], channels = [], tasks = []}} in

let s1 = nameSym "s1" in
let s2 = nameSym "s2" in
let a1 = nameSym "a1" in
let d1 = nameSym "d1" in
let period = nameSym "period" in
let d1in1 = nameSym "in1" in
let d1out = nameSym "out" in
let d2 = nameSym "d2" in
let d2in1 = nameSym "in1" in
let d2in2 = nameSym "in2" in
let d2out = nameSym "out" in
let a = nameSym "a" in
let b = nameSym "b" in
let c = nameSym "c" in

-- Encoding of a task example we constructed during a meeting.
let example = {
  tops = [
    Sensor {id = s1, ty = TFloat ()},
    Sensor {id = s2, ty = TFloat ()},
    Actuator {id = a1, ty = TFloat ()},
    Function {
      id = d1, params = [(period, TFloat ())], ty = TUnit (),
      body = ([Input (d1in1, TFloat ()), Output (d1out, TFloat ())], [])
    },
    Function {
      id = d2, params = [], ty = TUnit (),
      body = ([Input (d2in1, TFloat ()), Input (d2in2, TFloat ()), Output (d2out, TFloat ())], [])
    }
  ],
  main = {
    params = [],
    tasks = [
      {id = a, e = FunctionCall {id = d1, args = [Literal {const = CInt {i = 500}}]}},
      {id = b, e = FunctionCall {id = d2, args = []}},
      {id = c, e = FunctionCall {id = d2, args = []}}
    ],
    connections = [
      {from = ExtChannel s1, to = LocalChannel {task = a, id = d1in1}},
      {from = LocalChannel {task = a, id = d1out}, to = LocalChannel {task = b, id = d2in1}},
      {from = ExtChannel s2, to = LocalChannel {task = b, id = d2in2}},
      {from = LocalChannel {task = a, id = d1out}, to = LocalChannel {task = c, id = d2in1}},
      {from = LocalChannel {task = b, id = d2out}, to = LocalChannel {task = c, id = d2in2}},
      {from = LocalChannel {task = c, id = d2out}, to = ExtChannel a1}
    ]
  }
} in
printLn (pprintRtpplProgram example);

()
