include "arg.mc"
include "bool.mc"

type Options = {
  recording : Bool,
  replaying : Bool
}

let optionsDefault = {
  recording = false,
  replaying = false
}

let recordMsg = join [
  "Enables recording inputs to file. When enabled, all read input data is ",
  "stored in buffers. When the program is terminated, these buffers are ",
  "stored in files."
]

let replayMsg = join [
  "Enables replaying inputs from a file. When enabled, all read input data ",
  "is taken from files produced when recording."
]

let optionsConfig : ParseConfig Options = [
  ( [("--record", "", "")]
  , recordMsg
  , lam p : ArgPart Options. {p.options with recording = true} ),
  ( [("--replay", "", "")]
  , replayMsg
  , lam p : ArgPart Options. {p.options with replaying = true} )
]

let parseOptions : [String] -> Options = lam args.
  let result =
    argParse_general {args = args, optionsStartWith = []} optionsDefault optionsConfig
  in
  match result with ParseOK r then
    let options = r.options in
    if and options.recording options.replaying then 
      error "Cannot enable recording and replaying at the same time"
    else options
  else argPrintError result; exit 1
