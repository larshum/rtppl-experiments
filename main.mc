-- The main compiler for RTPPL

include "mexpr/ast.mc"
include "mexpr/remove-ascription.mc"
include "mexpr/shallow-patterns.mc"
include "mexpr/symbolize.mc"
include "mexpr/type-check.mc"
include "mexpr/utesttrans.mc"
include "ocaml/mcore.mc"

include "../coreppl/src/parser.mc"
include "rtppl.mc"

lang RTPPLCompile =
  RTPPL + BootParser + MExprSym + MExprTypeCheck +
  MExprRemoveTypeAscription + MExprUtestTrans + MCoreCompileLang
end

mexpr

use RTPPLCompile in

let rtpplKeywords = ["sdelay"] in

-- NOTE: this and the function below are taken from 'mi-lite.mc'. These should
-- probably be placed in a file in the standard library, and also they should
-- be improved to handle more cases (e.g. now they won't work with Windows paths).
let filename = lam path.
  match strLastIndex '/' path with Some idx then
    subsequence path (addi idx 1) (length path)
  else path
in

let filenameWithoutExtension = lam filename.
  match strLastIndex '.' filename with Some idx then
    subsequence filename 0 idx
  else filename
in

let generateTests = lam ast. lam enabled.
  if enabled then
    let ast = removeTypeAscription ast in
    utestGen ast
  else (symEnvEmpty, utestStrip ast)
in

-- TODO: use argument parser to customize parsing etc
let f = get argv 1 in
-- TODO: include DPPL keywords
let args = {defaultBootParserParseMCoreFileArg with keywords = rtpplKeywords} in
let ast = parseMCoreFile args f in
let ast = makeKeywords [] ast in

let ast = symbolize ast in
let ast = replaceSdelay ast in
let ast = typeCheck ast in

-- TODO: always enable/disable tests, or control via option?
match generateTests ast true with (symEnv, ast) in
let ast = symbolizeExpr symEnv ast in

let ast = use MExprLowerNestedPatterns in lowerAll ast in

let compileOcaml = lam libs. lam clibs. lam ocamlProg.
  let options = {optimize = true, libraries = libs, cLibraries = clibs} in
  let p = ocamlCompileWithConfig options ocamlProg in
  -- TODO: add flag to direct output?
  let destinationFile = filenameWithoutExtension (filename f) in
  sysMoveFile p.binaryPath destinationFile;
  sysChmodWriteAccessFile destinationFile;
  p.cleanup ();
  ()
in

compileMCore ast (mkEmptyHooks compileOcaml)
