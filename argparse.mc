include "arg.mc"
include "bool.mc"
include "set.mc"

type Options = {
  printFloat : Int,
  printDist : Int,
  printPosDist : Int,
  recording : Bool,
  replaying : Bool,
  roomMapFile : String,
  bufferOnlyOutputs : Set Int
}

let optionsDefault = {
  printFloat = negi 1,
  printDist = negi 1,
  printPosDist = negi 1,
  recording = false,
  replaying = false,
  roomMapFile = "",
  bufferOnlyOutputs = setEmpty subi
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

let printFloatMsg = "Prints the contents of a buffer containing floats."

let printDistMsg = "Prints the contents of a buffer containing a distribution of floats."

let printPosDistMsg = "Prints the contents of a buffer containing a position distribution."

let optionsConfig : ParseConfig Options = [
  ( [("--record", "", "")]
  , recordMsg
  , lam p : ArgPart Options. {p.options with recording = true} ),
  ( [("--replay", "", "")]
  , replayMsg
  , lam p : ArgPart Options. {p.options with replaying = true} ),
  ( [("--print-float", " ", "<index>")]
  , printFloatMsg
  , lam p : ArgPart Options. {p.options with printFloat = string2int (argToString p)} ),
  ( [("--print-dist", " ", "<index>")]
  , printDistMsg
  , lam p : ArgPart Options. {p.options with printDist = string2int (argToString p)} ),
  ( [("--print-pos-dist", " ", "<index>")]
  , printPosDistMsg
  , lam p : ArgPart Options. {p.options with printPosDist = string2int (argToString p)} ),
  ( [("--room-map", " ", "<file>")]
  , "Sets the file from which to read a map of a room"
  , lam p : ArgPart Options. {p.options with roomMapFile = argToString p} )
]

let parseOptions : [String] -> Options = lam args.
  let result =
    argParse_general {args = args, optionsStartWith = []} optionsDefault optionsConfig
  in
  match result with ParseOK r then
    let options = r.options in
    if and options.recording options.replaying then
      error "Cannot enable recording and replaying at the same time"
    else if and (neqi options.printFloat (negi 1)) (neqi options.printDist (negi 1)) then
      error "Can only print the contents of one buffer at a time"
    else options
  else argPrintError result; exit 1
