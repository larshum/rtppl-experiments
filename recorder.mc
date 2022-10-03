include "ext/rtppl-ext.mc"
include "common.mc"
include "option.mc"
include "string.mc"
include "sm_conf.mc"

-- Loop construct that repeatedly calls a function f while passing along an
-- accumulated value.
recursive let loopFn : all a. (a -> a) -> a -> a =
  lam f. lam v.
  let vnext = f v in
  loopFn f vnext
end

let ts2string : TimeStampedValue -> String = lam ts.
  join [int2string ts.0, " ", float2string ts.1]

let parseTs : String -> TimeStampedValue = lam line.
  match strSplit " " line with [ts, value] then
    (ts, value)
  else error "Invalid format of time-stamped value"

-- A memory buffer used to store time stamped values
type Buffer = {id : String, data : Ref [TimeStampedValue]}

let emptyBuffer : String -> Buffer = lam id.
  {id = id, data = ref (toList [])}

let loadBuffer : String -> Buffer = lam id.
  let s = readFile (join ["trace-", id, ".txt"]) in
  let data = map parseTs (strSplit "\n" s) in
  {id = id, data = ref (toList data)}

let addBuffer : Buffer -> TimeStampedValue -> () =
  lam buf. lam entry.
  modref buf.data (cons entry (deref buf.data))

let saveBuffer : Buffer -> () =
  lam buf.
  writeFile
    (join ["trace-", buf.id, ".txt"])
    (strJoin "\n" (map ts2string (reverse (deref buf.data))))

mexpr

let options = {isRecording = true} in

let buffers = {f1 = emptyBuffer "front-sensor-1", f2 = emptyBuffer "front-sensor-2"} in

let updateBuffersIfRecording = lam f1. lam f2.
  if options.isRecording then
    addBuffer buffers.f1 f1;
    addBuffer buffers.f2 f2
  else ()
in

-- Capture signal 2 (SIGINT) to write buffer to file before terminating, if we
-- are recording.
let saveBuffersAndExit : Signal -> () = lam.
  (if options.isRecording then
    saveBuffer buffers.f1;
    saveBuffer buffers.f2
  else ());
  exit 0
in
setSignalHandler 2 saveBuffersAndExit;

-- TODO: get these definitions from a generated header-file
let front1 = 0 in
let front2 = 1 in

-- Open channel for writing the distance estimation
-- let dist = openFdExn "estimate-distance" in

-- TODO: PPL prior (e.g. initial distance)
--let prior = sample (Uniform 0 10) in
let prior = () in

-- TODO: if needed, keep track of the latest timestamp and propagate it
-- "properly" (however we define that)

-- Main loop, which passes along a sampled value from the posterior in each
-- iteration.
loopFn (lam d.

  -- TODO: use a proper sdelay implementation
  sleepMs 20;

  let tsv1 = lvRead front1 in
  let tsv2 = lvRead front2 in

  updateBuffersIfRecording tsv1 tsv2;

  -- TODO: PPL model, sampling a new value d from posterior
  /-let d = infer (lam.
    observe f1;
    observe f2;
    assume (Dist ?) d
  )-/

  -- Write the observed distance to the output
  --let tsv = [ts1, d] in
  --writeFd dist td;

  d
) prior
