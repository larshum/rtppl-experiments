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
  outputs : [Int],
  bufferOnlyOutputs : Set Int
}

let _bufferFile = lam id. join ["trace-", int2string id]
let _replayBufferFile = lam id. join ["replay-", _bufferFile id]

let _initBuffer = lam. toList []

let _loadBuffer = lam id.
  match readOpen id with Some bufFile then
    let data : [TimeStampedValue] = unsafeCoerce (readBinary bufFile) in
    toList data
  else error (join ["Could not open buffer file ", id, " for reading"])

let _saveBuffer = lam id. lam tsvs.
  match writeOpen id with Some bufFile then
    writeBinary bufFile (reverse tsvs)
  else error (join ["Could not open buffer file ", id, " for writing"])

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

  sem loadInputBuffer : Map Int Buffer -> Int -> Mode -> Map Int Buffer
  sem loadInputBuffer buffers id =
  | Default _ -> buffers
  | Record _ -> mapInsert id (_initBuffer ()) buffers
  | Replay _ -> mapInsert id (_loadBuffer (_bufferFile id)) buffers

  sem loadOutputBuffer : Map Int Buffer -> Int -> Mode -> Map Int Buffer
  sem loadOutputBuffer buffers id =
  | Default _ -> buffers
  | Record _ | Replay _ ->
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
    { mode = mode, inputs = inputs, outputs = outputs, buffers = buffers
    , bufferOnlyOutputs = options.bufferOnlyOutputs }

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

  sem readInputBuffer : (Int -> (Int, Opaque)) -> Int -> BufferState -> Mode
                     -> (BufferState, TimeStampedValue)
  sem readInputBuffer readFn id state =
  | Default _ -> (state, readFn id)
  | Record _ ->
    let tsv = readFn id in
    let state = pushBuffer id state tsv in
    (state, tsv)
  | Replay _ -> popBuffer id state

  sem writeOutputBuffer : Int -> TimeStampedValue -> BufferState -> Mode -> BufferState
  sem writeOutputBuffer id tsv state =
  | Default _ -> lvWrite id tsv; state
  | Record _ ->
    -- NOTE(larshum, 2022-11-21): We do not write to the shared memory if the
    -- output buffer is marked as buffer only. This is used when we know the
    -- output will never be read from shared memory (e.g., the final posterior
    -- estimate).
    (if setMem id state.bufferOnlyOutputs then () else lvWrite id tsv);
    pushBuffer id state tsv
  | Replay _ ->
    pushBuffer id state tsv

  sem saveBuffersAndExit : all a. BufferState -> a
  sem saveBuffersAndExit =
  | state -> saveBuffers state state.mode; exit 0

  sem saveBuffers : BufferState -> Mode -> ()
  sem saveBuffers state =
  | Default _ -> ()
  | Replay _ ->
    -- When replaying, we store the values in the output buffers in separate
    -- files, and leave the input buffers the way they were before.
    let saveBufferH = lam id.
      match mapLookup id state.buffers with Some buf then
        _saveBuffer (_replayBufferFile id) buf
      else error (join ["Output buffer with id ", int2string id, " not found"])
    in
    iter saveBufferH state.outputs
  | Record _ ->
    -- When recording, we store both inputs and output buffer data.
    mapMapWithKey saveBuffer state.buffers ; ()

  sem saveBuffer : Int -> [TimeStampedValue] -> ()
  sem saveBuffer id =
  | tsvs -> _saveBuffer (_bufferFile id) tsvs
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

let readData = lam lvReadFn. lam id.
  use RTPPLBuffers in
  let state = _getState () in
  match readInputBuffer lvReadFn id state state.mode with (state, tsv) in
  modref _bufferState (Some state);
  tsv

let readFloatData : Int -> (Int, Float) = lam id.
  use RTPPLBuffers in
  match readData lvReadFloat id with (ts, value) in
  (ts, unsafeCoerce value)

let readDistData : Int -> (Int, Dist Float) = lam id.
  use RTPPLBuffers in
  match readData lvRead id with (ts, value) in
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
    saveBuffersAndExit state; ()
  else exit 0

-- Sets the above function to be called on the SIGINT signal.
let __ignored = setSignalHandler 2 saveBuffersAndExit

