include "ext/file-ext.mc"
include "ext/rtppl-ext.mc"
include "common.mc"
include "map.mc"
include "option.mc"
include "string.mc"

include "argparse.mc"
include "sm_conf.mc"

type Buffer = [TimeStampedValue]

type BufferState = {
  mode : Mode,
  buffers : Map Int Buffer,
  inputs : [Int],
  outputs : [Int]
}

let _bufferFile = lam id. join ["trace-", int2string id, ".txt"]

let _initBuffer = lam. toList []

let _loadBuffer = lam id.
  match readOpen (_bufferFile id) with Some bufFile then
    let data : [TimeStampedValue] = unsafeCoerce (readBinary bufFile) in
    toList data
  else error (join ["Could not open buffer file ", _bufferFile id, " for reading"])

let _saveBuffer = lam id. lam tsvs.
  match writeOpen (_bufferFile id) with Some bufFile then
    writeBinary bufFile (reverse tsvs)
  else error (join ["Could not open buffer file ", _bufferFile id, " for writing"])

lang RTPPLBuffers
  syn Mode =
  | Default ()
  | Record ()
  | RecordBufferOnly ()
  | Replay ()

  sem selectMode : Options -> Mode
  sem selectMode =
  | options ->
    if options.recording then
      if options.recordBufferOnly then RecordBufferOnly ()
      else Record ()
    else if options.replaying then Replay ()
    else Default ()

  sem loadInputBuffer : Map Int Buffer -> Int -> Mode -> Map Int Buffer
  sem loadInputBuffer buffers id =
  | Default _ -> buffers
  | Record _ | RecordBufferOnly _ -> mapInsert id (_initBuffer ()) buffers
  | Replay _ -> mapInsert id (_loadBuffer id) buffers

  sem loadOutputBuffer : Map Int Buffer -> Int -> Mode -> Map Int Buffer
  sem loadOutputBuffer buffers id =
  | Default _ -> buffers
  | Record _ | RecordBufferOnly _ | Replay _ ->
    mapInsert id (_initBuffer ()) buffers

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
  | RecordBufferOnly _ ->
    error "Cannot read data when recording in buffer-only mode"

  sem writeOutputBuffer : Int -> TimeStampedValue -> BufferState -> Mode -> BufferState
  sem writeOutputBuffer id tsv state =
  | Default _ -> lvWrite id tsv; state
  | Record _ ->
    lvWrite id tsv;
    pushBuffer id state tsv
  | RecordBufferOnly _ | Replay _ ->
    pushBuffer id state tsv

  sem saveBuffersAndExit : all a. BufferState -> a
  sem saveBuffersAndExit =
  | state -> saveBuffers state state.mode; exit 0

  sem saveBuffers : BufferState -> Mode -> ()
  sem saveBuffers state =
  | Default _ -> ()
  | Replay _ ->
    -- When replaying, we store the values in the output buffers but leave the
    -- input buffers the way they were before.
    let saveBufferH = lam id.
      match mapLookup id state.buffers with Some buf then
        saveBuffer id buf
      else error (join ["Output buffer with id ", int2string id, " not found"])
    in
    iter saveBufferH state.outputs
  | Record _ | RecordBufferOnly _ ->
    -- When recording, we store both inputs and output buffer data.
    mapMapWithKey saveBuffer state.buffers ; ()

  sem saveBuffer : Int -> [TimeStampedValue] -> ()
  sem saveBuffer id =
  | tsvs -> _saveBuffer id tsvs
end

-- Overwrite the above definitions with ones that operate on a mutable
-- reference, so that we can use it in a signal handler function.
let _bufferState = ref (None ())

let _getState : () -> BufferState = lam.
  match deref _bufferState with Some state then
    state
  else error "Buffer state error"

let initBuffers = lam options. lam inputs. lam outputs.
  use RTPPLBuffers in
  let state = init options inputs outputs in
  modref _bufferState (Some state);
  state

let readData = lam id.
  use RTPPLBuffers in
  let state = _getState () in
  match readInputBuffer id state state.mode with (state, tsv) in
  modref _bufferState (Some state);
  tsv

let readFloatData : Int -> (Int, Float) = lam id.
  use RTPPLBuffers in
  match readData id with (ts, value) in
  (ts, unsafeCoerce value)

let readDistData : Int -> (Int, Dist Float) = lam id.
  use RTPPLBuffers in
  match readData id with (ts, value) in
  (ts, unsafeCoerce value)

let writeData : all a. Int -> (Int, a) -> () = lam id. lam outputData.
  use RTPPLBuffers in
  let state = _getState () in
  let state = writeOutputBuffer id (unsafeCoerce outputData) state state.mode in
  modref _bufferState (Some state);
  ()

let saveBuffersAndExit : Signal -> () = lam.
  use RTPPLBuffers in
  match deref _bufferState with Some state then
    saveBuffersAndExit state
  else exit 0

-- Sets the above function to be called on the SIGINT signal.
let __ignored = setSignalHandler 2 saveBuffersAndExit

