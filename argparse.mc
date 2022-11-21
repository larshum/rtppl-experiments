include "arg.mc"
include "bool.mc"
include "set.mc"

type BufferType
con FloatBuffer : () -> BufferType
con DistFloatBuffer : () -> BufferType
con DistPosBuffer : () -> BufferType

type Options = {
  printBufferFiles : [(String, BufferType)],
  recording : Bool,
  replaying : Bool,
  roomMapFile : String,
  bufferOnlyOutputs : Set Int
}

let optionsDefault = {
  printBufferFiles = [],
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

let printFloatMsg = "Prints the contents of the chosen buffer file containing floats."

let printDistMsg = "Prints the contents of the chosen buffer file containing float distributions."

let printPosDistMsg = "Prints the contents of the chosen buffer file containing position distributions."

let optionsConfig : ParseConfig Options = [
  ( [("--record", "", "")]
  , recordMsg
  , lam p : ArgPart Options. {p.options with recording = true} ),
  ( [("--replay", "", "")]
  , replayMsg
  , lam p : ArgPart Options. {p.options with replaying = true} ),
  ( [("--print-float", " ", "<file>")]
  , printFloatMsg
  , lam p : ArgPart Options.
      let entry = (argToString p, FloatBuffer ()) in
      {p.options with printBufferFiles = snoc p.options.printBufferFiles entry} ),
  ( [("--print-dist", " ", "<file>")]
  , printDistMsg
  , lam p : ArgPart Options.
      let entry = (argToString p, DistFloatBuffer ()) in
      {p.options with printBufferFiles = snoc p.options.printBufferFiles entry} ),
  ( [("--print-pos-dist", " ", "<file>")]
  , printPosDistMsg
  , lam p : ArgPart Options.
      let entry = (argToString p, DistPosBuffer ()) in
      {p.options with printBufferFiles = snoc p.options.printBufferFiles entry} ),
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
    else options
  else argPrintError result; exit 1
