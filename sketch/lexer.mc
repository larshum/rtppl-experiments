include "parser/lexer.mc"

lang BooleanTokenParser = TokenParser
  syn Token =
  | BoolTok {info : Info, val : Bool}

  syn TokenRepr =
  | BoolRepr ()

  sem parseToken pos =
  | "true" ++ str ->
    let pos2 = advanceCol pos 4 in
    let info = makeInfo pos pos2 in
    { token = BoolTok {info = info, val = true}
    , lit = ""
    , info = info
    , stream = {pos = pos2, str = str} }
  | "false" ++ str ->
    let pos2 = advanceCol pos 5 in
    let info = makeInfo pos pos2 in
    { token = BoolTok {info = info, val = false}
    , lit = ""
    , info = info
    , stream = {pos = pos2, str = str} }

  sem tokKindEq tokRepr =
  | BoolTok _ -> match tokRepr with BoolRepr _ then true else false

  sem tokInfo =
  | BoolTok {info = info} -> info

  sem tokToStr =
  | BoolTok {v = v} -> concat "<Bool>" (if v then "true" else "false")

  sem tokReprToStr =
  | BoolRepr _ -> "<Bool>"

  sem tokToRepr =
  | BoolTok _ -> BoolRepr ()
end

lang RTPPLLineCommentParser = WSACParser
  sem eatWSAC p =
  | "//" ++ xs -> eatComment p xs

  sem eatComment : Pos -> String -> {pos : Pos, str : String}
  sem eatComment p =
  | "\n" ++ xs -> eatWSAC (advanceRow p 1) xs
  | [_] ++ xs -> eatComment (advanceCol p 1) xs
  | _ -> eatWSAC p ""
end
