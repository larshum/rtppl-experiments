include "mexpr/anf.mc"
include "mexpr/ast.mc"
include "mexpr/duplicate-code-elimination.mc"
include "mexpr/eq.mc"
include "mexpr/keyword-maker.mc"
include "mexpr/pprint.mc"
include "mexpr/symbolize.mc"
include "mexpr/type-lift.mc"
include "mexpr/type-check.mc"
include "mexpr/utils.mc"

lang RTPPLAst =
  Ast + PrettyPrint + Eq + Sym + TypeCheck + ANF + TypeAnnot + TypeLift + KeywordMaker

  syn Expr =
  | TmSdelay {millis : Int, ty : Type, info : Info}

  sem infoTm =
  | TmSdelay t -> t.info

  sem tyTm =
  | TmSdelay t -> t.ty

  sem withInfo info =
  | TmSdelay t -> TmSdelay {t with info = info}

  sem withType ty =
  | TmSdelay t -> TmSdelay {t with ty = ty}

  sem smapAccumL_Expr_Expr f acc =
  | TmSdelay t -> (acc, TmSdelay t)

  sem isAtomic =
  | TmSdelay _ -> true

  sem pprintCode indent env =
  | TmSdelay t ->
    (env, join ["sdelay ", int2string t.millis])

  sem eqExprH env free lhs =
  | TmSdelay r ->
    match lhs with TmSdelay l then
      if eqi l.millis r.mills then Some free
      else None ()
    else None ()

  sem symbolizeExpr env =
  | TmSdelay t -> TmSdelay t

  sem typeAnnotExpr env =
  | TmSdelay t ->
    let tyint = TyInt {info = t.info} in
    TmSdelay {t with ty = TyArrow {from = tyint, to = tyint, info = t.info}}

  sem typeCheckExpr env =
  | TmSdelay t ->
    let tyint = TyInt {info = t.info} in
    TmSdelay {t with ty = TyArrow {from = tyint, to = tyint, info = t.info}}

  sem normalize k =
  | TmSdelay t -> TmSdelay t

  sem typeLiftExpr env =
  | TmSdelay t -> (env, TmSdelay t)

  sem isKeyword =
  | TmSdelay _ -> true

  sem _parseMillis : Info -> Expr -> Int
  sem _parseMillis info =
  | TmConst {val = CInt {val = i}} -> i
  | _ ->
    errorSingle [info] "Expected sdelay argument to be an integer literal"

  sem matchKeywordString info =
  | "sdelay" ->
    Some (1, lam lst. TmSdelay {millis = _parseMillis info (get lst 0),
                                ty = TyUnknown {info = info}, 
                                info = info})
end

lang RTPPL = RTPPLAst + MExprEliminateDuplicateCode + MExprFindSym + BootParser
  sem replaceSdelay : Expr -> Expr
  sem replaceSdelay =
  | t ->
    let sdelayRuntime = loadRuntime "rtppl-runtime.mc" in
    match findNamesOfStrings ["startTimeInit", "delayBy"] sdelayRuntime
    with [Some startTimeInitId, Some delayById] then
      let astWithRuntime =
        bind_
          sdelayRuntime
          (replaceSdelayH (startTimeInitId, delayById) t) in
      eliminateDuplicateCode astWithRuntime
    else error "Error while loading RTPPL runtime"

  sem loadRuntime : String -> Expr
  sem loadRuntime =
  | file ->
    let parse = parseMCoreFile {
      defaultBootParserParseMCoreFileArg with
        keepUtests = false, eliminateDeadCode = false, allowFree = true
    } in
    -- TODO: does this always work, or only when executing in this dir?
    let runtime = parse file in
    symbolizeExpr {symEnvEmpty with allowFree = true} runtime


  sem replaceSdelayH : (Name, Name) -> Expr -> Expr
  sem replaceSdelayH rtIds =
  | TmLam (t & {body = !TmLam _}) ->
    -- If the body of the innermost lambda contains use of sdelay, we add
    -- timing points to it.
    let body =
      if bodyHasSdelay false t.body then
        let tpId = nameSym "tp" in
        match rtIds with (startTimeInitId, delayById) in
        let delayFn = app_ (nvar_ delayById) (nvar_ tpId) in
        let body = transformSdelay delayFn t.body in
        bind_ (nulet_ tpId (app_ (nvar_ startTimeInitId) unit_)) body
      else replaceSdelayH rtIds t.body
    in
    TmLam {t with body = body}
  | t -> smap_Expr_Expr (replaceSdelayH rtIds) t

  sem bodyHasSdelay : Bool -> Expr -> Bool
  sem bodyHasSdelay acc =
  | TmSdelay _ -> true
  | t -> if acc then true else sfold_Expr_Expr bodyHasSdelay false t

  sem transformSdelay : Expr -> Expr -> Expr
  sem transformSdelay delayFn =
  | TmLam t -> TmLam t
  | TmSdelay t -> app_ delayFn (int_ t.millis)
  | t -> smap_Expr_Expr (transformSdelay delayFn) t
end
