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
    join ["acutator ", nameGetStr id, " : ", pprintRtpplType ty]
  | FunctionDefTop {id = {v = id}, params = params, ty = ty, body = {chs = channels, stmts = stmts}} ->
    let paramsStr = pprintRtpplParams params in
    let chs = strJoin "" (map (lam ch. concat (pprintRtpplChannel ch) "\n") channels) in
    let stmtStrs = strJoin "\n" (map (pprintRtpplStmt 2) stmts) in
    join ["def ", nameGetStr id, "(", paramsStr, ") : ", pprintRtpplType ty,
          " {\n", chs, stmtStrs, "}"]

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

  sem pprintRtpplStmt : Int -> Stmt -> String
  sem pprintRtpplStmt indent =

  sem pprintRtpplExpr : Int -> Expr -> String
  sem pprintRtpplExpr indent =
  | FunctionCallExpr {id = {v = id}, args = args} ->
    join [nameGetStr id, "(", pprintRtpplArgs indent args, ")"]
  | LiteralExpr {const = c} -> pprintRtpplConst c

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
  | LitBoolConst _ -> ""

  sem pprintRtpplType : Type -> String
  sem pprintRtpplType =
  | IntType _ -> "Int"
  | FloatType _ -> "Float"
  | BoolType _ -> "Bool"
  | UnitType _ -> "Unit"
end

mexpr

use RtpplPrettyPrint in

let input = "tasks.rpl" in
let content = readFile input in
let program = parseRtpplExn input content in
printLn (pprintRtpplProgram program)
