include "parser/lexer.mc"

lang BooleanTokenParser = TokenParser
  syn Token =
  | BoolTok {info : Info, val : Bool}
  syn TokenRepr =
  | BoolRepr ()

  sem parseToken pos =
  | ("true" ++ _) & str ->
    let pos2 = advanceCol pos 4 in
    let info = makeInfo pos pos2 in
    { token = BoolTok {info = info, val = true}
    , lit = ""
    , info = info
    , stream = {pos = pos2, str = str} }
  | ("false" ++ _) & str ->
    let pos2 = advanceCol pos 5 in
    let info = makeInfo pos pos2 in
    { token = BoolTok {info = info, val = false}
    , lit = ""
    , info = info
    , stream = {pos = pos2, str = str} }
end
