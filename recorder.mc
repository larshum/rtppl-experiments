include "ext/rtppl-ext.mc"
include "common.mc"
include "option.mc"
include "string.mc"

-- Loop construct that repeatedly calls a function f until the accumulated
-- optional value turns out to be None ().
recursive let loopFn : all a. (a -> Option a) -> a -> a =
  lam f. lam v.
  match f v with Some vnext then
    loopFn f vnext
  else v
end

let ts2string : TimeStampedValue -> String = lam ts.
  strJoin " " (map float2string ts)

let parseTs : String -> TimeStampedValue = lam s.
  map string2float (strSplit " " s)

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

let buffers = {f1 = emptyBuffer "fs1", f2 = emptyBuffer "fs2"} in

-- For the moment, we always record data
let updateBuffersIfRecording = lam f1. lam f2.
  if options.isRecording then
    addBuffer buffers.f1 f1;
    addBuffer buffers.f2 f2
  else ()
in

let saveBuffersIfRecording = lam.
  if options.isRecording then
    saveBuffer buffers.f1;
    saveBuffer buffers.f2
  else ()
in

let openFdExn = lam s.
  let fd = openFd s in
  if eqi fd (negi 1) then
    error (join ["Could not get file descriptor of ", s])
  else fd
in

-- Open channels for reading data from the front sensors
let front1 = openFdExn "front-sensor-1" in
let front2 = openFdExn "front-sensor-2" in

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

  let tsv1 = readFd front1 2 in
  let tsv2 = readFd front2 2 in

  -- Stop the loop if we read incomplete data.
  if or (neqi (length tsv1) 2) (neqi (length tsv2) 2) then None ()
  else

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

  Some d
) prior;

saveBuffersIfRecording ()
