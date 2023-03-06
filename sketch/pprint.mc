include "rtppl.mc"

lang RtpplPrettyPrint = RtpplAst
  sem pprintRtpplProgram : Program -> String
  sem pprintRtpplProgram =
  | Program p ->
    let topsStr = strJoin "\n" (map pprintRtpplTop p.tops) in
    let mainStr = pprintRtpplMain p.main in
    join [topsStr, "\n", mainStr]

  sem pprintRtpplTop : Top -> String
  sem pprintRtpplTop =
  | SensorTop {id = {v = id}, ty = ty} ->
    join ["sensor ", nameGetStr id, " : ", pprintRtpplType ty]
  | ActuatorTop {id = {v = id}, ty = ty} ->
    join ["actuator ", nameGetStr id, " : ", pprintRtpplType ty]
  | ConstantTop {id = {v = id}, ty = ty, e = e} ->
    join ["const ", nameGetStr id, " : ", pprintRtpplType ty, " = ", pprintRtpplExpr 2 e]
  | TypeAliasTop {id = {v = id}, ty = ty} ->
    join ["type ", nameGetStr id, " = ", pprintRtpplType ty]
  | FunctionDefTop {id = {v = id}, params = params, ty = ty, body = {chs = channels, stmts = stmts}} ->
    let paramsStr = pprintRtpplParams params in
    let chs = strJoin "" (map (lam ch. concat (pprintRtpplChannel ch) "\n") channels) in
    let stmtStrs = strJoin "\n" (map (pprintRtpplStmt 2) stmts) in
    join ["def ", nameGetStr id, "(", paramsStr, ") : ", pprintRtpplType ty,
          " {\n", chs, stmtStrs, "\n}"]

  sem pprintRtpplChannel : Channel -> String
  sem pprintRtpplChannel =
  | InputChannel {id = {v = id}, ty = ty} ->
    join ["  input ", nameGetStr id, " : ", pprintRtpplType ty]
  | OutputChannel {id = {v = id}, ty = ty} ->
    join ["  output ", nameGetStr id, " : ", pprintRtpplType ty]

  sem pprintRtpplParams : [{id : {i : Info, v : Name}, ty : Type}] -> String
  sem pprintRtpplParams =
  | [{id = {v = id}, ty = ty}] ++ params ->
    let tailstr =
      if null params then ""
      else concat ", " (pprintRtpplParams params)
    in
    join [nameGetStr id, " : ", pprintRtpplType ty, tailstr]
  | [] -> ""

  sem pprintRtpplMain : Main -> String
  sem pprintRtpplMain =
  | Main {params = params, tasks = tasks, connections = connections} ->
    let paramsStr = pprintRtpplParams params in
    let tasksStr = strJoin "\n" (map pprintRtpplTask tasks) in
    let connectionsStr = strJoin "\n" (map pprintRtpplConnection connections) in
    join ["main(", paramsStr, ") {\n", tasksStr, "\n", connectionsStr, "\n}"]

  sem pprintRtpplTask : Task -> String
  sem pprintRtpplTask =
  | Task {id = {v = id}, e = e} ->
    join ["  task ", nameGetStr id, " = ", pprintRtpplExpr 2 e]

  sem pprintRtpplConnection : Connection -> String
  sem pprintRtpplConnection =
  | Connection {from = from, to = to} ->
    let pprintSpec = lam spec.
      match spec with ChannelSpec {task = {v = tid}, id = id} in
      match id with Some {v = chid} then
        join [nameGetStr tid, ".", nameGetStr chid]
      else nameGetStr tid
    in
    join ["  ", pprintSpec from, " -> ", pprintSpec to]

  sem pprintIndent : Int -> String
  sem pprintIndent =
  | n -> create n (lam. ' ')

  sem pprintIndentIncrement : Int -> Int
  sem pprintIndentIncrement =
  | n -> addi n 2

  sem pprintNewline : Int -> String
  sem pprintNewline =
  | n -> cons '\n' (create n (lam. ' '))

  sem pprintRtpplStmt : Int -> Stmt -> String
  sem pprintRtpplStmt indent =
  | BindingStmt {id = {v = id}, ty = ty, e = e} ->
    join [pprintIndent indent, "var ", nameGetStr id, " : ",
          pprintRtpplType ty, " = ", pprintRtpplExpr indent e]
  | ForInStmt {id = {v = id}, e = e, body = body} ->
    let ii = pprintIndentIncrement indent in
    join [pprintIndent indent, "for ", nameGetStr id, " in ",
          pprintRtpplExpr indent e, " {\n",
          strJoin "\n" (map (pprintRtpplStmt ii) body),
          pprintNewline indent, "}"]
  | LoopStmt {body = body} ->
    let ii = pprintIndentIncrement indent in
    join [pprintIndent indent, "loop {\n",
          strJoin "\n" (map (pprintRtpplStmt ii) body),
          pprintNewline indent, "}"]
  | IdentPlusStmt {id = {v = id}, next = ReassignStmtNoIdent {proj = None _, e = e}} ->
    join [pprintIndent indent, nameGetStr id, " = ", pprintRtpplExpr indent e]
  | IdentPlusStmt {id = {v = id}, next = ReassignStmtNoIdent {proj = Some {v = pid}, e = e}} ->
    join [pprintIndent indent, nameGetStr id, ".", pid, " = ", pprintRtpplExpr indent e]
  | IdentPlusStmt {id = {v = id}, next = FunctionCallSStmtNoIdent {args = args}} ->
    let ii = pprintIndentIncrement indent in
    join [pprintIndent indent, nameGetStr id, "(",
          strJoin ", " (map (pprintRtpplExpr ii) args), ")"]
  | ConditionStmt {cond = cond, thn = thn, els = els} ->
    let ii = pprintIndentIncrement indent in
    join [pprintIndent indent, "if ", pprintRtpplExpr indent cond, " {\n",
          strJoin "\n" (map (pprintRtpplStmt ii) thn), "\n",
          pprintIndent indent, "} else {\n",
          strJoin "\n" (map (pprintRtpplStmt ii) els), "\n",
          pprintIndent indent, "}"]
  | ReturnStmt {e = e} ->
    join [pprintIndent indent, "return ", pprintRtpplExpr indent e]
  | BreakStmt _ -> concat (pprintIndent indent) "break"

  sem pprintRtpplExpr : Int -> Expr -> String
  sem pprintRtpplExpr indent =
  | IdentPlusExpr {id = {v = id}, next = VariableExprNoIdent _} -> nameGetStr id
  | IdentPlusExpr {id = {v = id}, next = FunctionCallEExprNoIdent {args = args}} ->
    join [nameGetStr id, "(", pprintRtpplArgs indent args, ")"]
  | IdentPlusExpr {id = {v = id}, next = ProjectionExprNoIdent {id = {v = projId}}} ->
    join [nameGetStr id, ".", projId]
  | LiteralExpr {const = c} -> pprintRtpplConst c
  | AddExpr {left = l, right = r} ->
    join [pprintRtpplExpr indent l, " + ", pprintRtpplExpr indent r]
  | SubExpr {left = l, right = r} ->
    join [pprintRtpplExpr indent l, " - ", pprintRtpplExpr indent r]
  | MulExpr {left = l, right = r} ->
    join [pprintRtpplExpr indent l, " * ", pprintRtpplExpr indent r]
  | DivExpr {left = l, right = r} ->
    join [pprintRtpplExpr indent l, " / ", pprintRtpplExpr indent r]
  | RemExpr {left = l, right = r} ->
    join [pprintRtpplExpr indent l, " % ", pprintRtpplExpr indent r]
  | EqExpr {left = l, right = r} ->
    join [pprintRtpplExpr indent l, " == ", pprintRtpplExpr indent r]
  | GeqExpr {left = l, right = r} ->
    join [pprintRtpplExpr indent l, " >= ", pprintRtpplExpr indent r]
  | LtExpr {left = l, right = r} ->
    join [pprintRtpplExpr indent l, " < ", pprintRtpplExpr indent r]
  | GtExpr {left = l, right = r} ->
    join [pprintRtpplExpr indent l, " > ", pprintRtpplExpr indent r]
  | AndExpr {left = l, right = r} ->
    join [pprintRtpplExpr indent l, " && ", pprintRtpplExpr indent r]
  | RecordLitExpr {fields = fields} ->
    let ii = pprintIndentIncrement indent in
    let ppField = lam f.
      match f with {id = {v = id}, e = e} in
      join [id, " = ", pprintRtpplExpr ii e]
    in
    join ["{", strJoin ", " (map ppField fields), "}"]
  | ArrayLitExpr {elems = elems} ->
    join ["[", strJoin ", " (map (pprintRtpplExpr indent) elems), "]"]
  | ArrayAccessExpr {e = e, idx = idx} ->
    join [pprintRtpplExpr indent e, "[", pprintRtpplExpr indent idx, "]"]

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
  | LitIntConst {value = {v = i}} -> int2string i
  | LitFloatConst {value = {v = f}} -> float2string f
  | LitBoolConst {value = {v = b}} -> if b then "true" else "false"
  | LitStringConst {value = {v = s}} -> join ["\"", escapeString s, "\""]

  sem pprintRtpplType : Type -> String
  sem pprintRtpplType =
  | IntType _ -> "Int"
  | FloatType _ -> "Float"
  | BoolType _ -> "Bool"
  | UnitType _ -> "Unit"
  | SeqType {ty = ty} -> join ["[", pprintRtpplType ty, "]"]
  | AliasType {id = {v = id}, next = DirectTypeNoIdent _} -> nameGetStr id
  | AliasType {id = {v = id}, next = ApplicationTypeNoIdent {args = args}} ->
    join [nameGetStr id, "(", strJoin ", " (map pprintRtpplType args), ")"]
  | RecordType {fields = fields} ->
    let ppField = lam f.
      match f with {id = {v = id}, ty = ty} in
      join [id, " : ", pprintRtpplType ty]
    in
    join ["{", strJoin ", " (map ppField fields), "}"]
  | FunctionType {from = from, to = to} ->
    join ["(", pprintRtpplType from, ") -> ", pprintRtpplType to]
end

mexpr

use RtpplPrettyPrint in

let input = get argv 1 in
let content = readFile input in
let program = parseRtpplExn input content in
printLn (pprintRtpplProgram program)
