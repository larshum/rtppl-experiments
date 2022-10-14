include "ext/rtppl-ext.mc"
include "common.mc"
include "map.mc"
include "option.mc"
include "string.mc"

include "argparse.mc"
include "sm_conf.mc"

-- TODO: How do we generalize these in case the time-stamped values can have
-- different types?
let parseTsv = lam s.
  match strSplit " " s with [ts, val] then
    (string2int ts, string2float val)
  else error "Invalid format of timestamp value"

let tsvToString = lam tsv.
  join [int2string tsv.0, " ", float2string tsv.1]

let _bufferFile = lam id. join ["trace-", int2string id, ".txt"]

let _initBuffer = lam. toList []

let _loadBuffer = lam id.
  let s = readFile (_bufferFile id) in
  let data = map parseTsv (strSplit "\n" s) in
  toList data

let _saveBuffer = lam id. lam tsvs.
  let s = strJoin "\n" (map tsvToString tsvs) in
  writeFile (_bufferFile id) s

lang RTPPLBuffers
  syn Mode =
  | Default ()
  | Record ()
  | Replay ()

  sem selectMode : Options -> Mode
  sem selectMode =
  | options ->
    if options.recording then Record ()
    else if options.replaying then Replay ()
    else Default ()

  type Buffer = [TimeStampedValue]

  type BufferState = {
    mode : Mode,
    buffers : Map Int Buffer,
    inputs : [Int],
    outputs : [Int]
  }

  sem loadInputBuffer : Map Int Buffer -> Int -> Mode -> Map Int Buffer
  sem loadInputBuffer buffers id =
  | Default _ -> buffers
  | Record _ -> mapInsert id (_initBuffer ()) buffers
  | Replay _ -> mapInsert id (_loadBuffer id) buffers

  sem loadOutputBuffer : Map Int Buffer -> Int -> Mode -> Map Int Buffer
  sem loadOutputBuffer buffers id =
  | Default _ -> buffers
  | Record _ | Replay _ -> mapInsert id (_initBuffer ()) buffers

  sem init : Options -> [Int] -> [Int] -> BufferState
  sem init options inputs =
  | outputs ->
    let mode = selectMode options in
    let buffers =
      foldl
        (lam buffers. lam inputId. loadInputBuffer buffers inputId mode)
        (mapEmpty subi) inputs in
    let buffers =
      foldl
        (lam buffers. lam outputId. loadOutputBuffer buffers outputId mode)
        buffers outputs in
    {mode = mode, inputs = inputs, outputs = outputs, buffers = buffers}

  sem pushBuffer : Int -> BufferState -> TimeStampedValue -> BufferState
  sem pushBuffer id state =
  | tsv ->
    match mapLookup id state.buffers with Some buf then
      let buf = cons tsv buf in
      {state with buffers = mapInsert id buf state.buffers}
    else error "Buffer with provided ID not found in buffer state"

  sem popBuffer : Int -> BufferState -> (BufferState, TimeStampedValue)
  sem popBuffer id =
  | state ->
    match mapLookup id state.buffers with Some buf then
      match buf with [tsv] ++ buf then
        ( {state with buffers = mapInsert id buf state.buffers}, tsv )
      else saveBuffersAndExit state
    else error "Buffer with provided ID not found in buffer state"

  sem readInputBuffer : Int -> BufferState -> Mode -> (BufferState, TimeStampedValue)
  sem readInputBuffer id state =
  | Default _ -> (state, lvRead id)
  | Record _ ->
    let tsv = lvRead id in
    let state = pushBuffer id state tsv in
    (state, tsv)
  | Replay _ -> popBuffer id state

  sem readData : BufferState -> (BufferState, [TimeStampedValue])
  sem readData =
  | state ->
    mapAccumL
      (lam state. lam inputId. readInputBuffer inputId state state.mode)
      state state.inputs

  sem writeOutputBuffer : Int -> TimeStampedValue -> BufferState -> Mode -> BufferState
  sem writeOutputBuffer id tsv state =
  | Default _ | Replay _ -> lvWrite id tsv; state
  | Record _ ->
    lvWrite id tsv;
    pushBuffer id state tsv

  sem writeData : BufferState -> [(Int, TimeStampedValue)] -> BufferState
  sem writeData state =
  | outputData ->
    foldl
      (lam state. lam data. writeOutputBuffer data.0 data.1 state state.mode)
      state outputData

  sem saveBuffersAndExit : all a. BufferState -> a
  sem saveBuffersAndExit =
  | state -> saveBuffers state state.mode; exit 0

  sem saveBuffers : BufferState -> Mode -> ()
  sem saveBuffers state =
  | Default _ | Replay _ -> ()
  | Record _ -> mapMapWithKey saveBuffer state.buffers ; ()

  sem saveBuffer : Int -> [TimeStampedValue] -> ()
  sem saveBuffer id =
  | tsvs -> _saveBuffer id tsvs
end

-- Overwrite the above definitions with ones that operate on a mutable
-- reference, so that we can use it in a signal handler function.
let _bufferState = ref (None ())

let _getState = lam.
  match deref _bufferState with Some state then
    state
  else error "Buffer state error"

let init = lam options. lam inputs. lam outputs.
  use RTPPLBuffers in
  let state = init options inputs outputs in
  modref _bufferState (Some state);
  state

let readData = lam.
  use RTPPLBuffers in
  let state = _getState () in
  match readData state with (state, tsvs) in
  modref _bufferState (Some state);
  tsvs

let writeData = lam outputData.
  use RTPPLBuffers in
  let state = _getState () in
  let state = writeData state outputData in
  modref _bufferState (Some state);
  ()

let saveBuffersAndExit : Signal -> () = lam.
  use RTPPLBuffers in
  match deref _bufferState with Some state then
    saveBuffersAndExit state
  else exit 0

-- Sets the above function to be called on the SIGINT signal.
let _dummy = setSignalHandler 2 saveBuffersAndExit

