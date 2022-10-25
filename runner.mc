include "ext/rtppl-ext.mc"
include "common.mc"
include "option.mc"
include "string.mc"

include "argparse.mc"
include "buffers.mc"
include "distance-model.mc"
include "sm_conf.mc"

let loopFn : all a. a -> (Int -> a -> a) -> a = lam v. lam f.
  recursive let work = lam i. lam v.
    let vnext = f i v in
    work (addi i 1) vnext
  in
  work 1 v

mexpr

let options = parseOptions (tail argv) in

-- Prints the contents of buffers by (unsafely) assuming the types of the
-- values they contain.
if neqi options.printFloat (negi 1) then
  let buf = _loadBuffer options.printFloat in
  let printTsv = lam tsv.
    match tsv with (ts, value) in
    join [int2string ts, " ", float2string (unsafeCoerce value)]
  in
  printLn (strJoin "\n" (map printTsv buf));
  exit 0
else if neqi options.printDist (negi 1) then
  let buf = _loadBuffer options.printDist in
  let printTsv = lam tsv.
    match tsv with (ts, dist) in
    printError (concat (int2string ts) "\n");
    let dist : Dist Float = unsafeCoerce dist in
    match distEmpiricalSamples dist with (samples, weights) in
    recursive let work = lam samples. lam weights.
      match (samples, weights) with ([s] ++ samples, [w] ++ weights) then
        printLn (join [float2string s, " ", float2string w]);
        work samples weights
      else ()
    in work samples weights
  in
  iter printTsv buf;
  exit 0
else

-- User has to declare the input and output sensors explicitly.
let inputs = [distanceFrontLeft, distanceFrontRight, speedValLeft, speedValRight] in
let outputs = [obsDistanceFront] in
initBuffers options inputs outputs;

let cmpTsv : (Int, Float) -> (Int, Float) -> Int = lam l. lam r.
  if gtf l.1 r.1 then 1
  else if ltf l.1 r.1 then negi 1
  else 0
in

-- Finds the median among a given sequence of observations. If there is an even
-- number of observations, the median is given the minimum timestamp among the
-- two considered values.
let median : [(Int, Float)] -> (Int, Float) = lam obs.
  let n = length obs in
  let obs = sort cmpTsv obs in
  if eqi (modi n 2) 0 then
    let fst = divi n 2 in
    match get obs fst with (ts1, v1) in
    match get obs (addi fst 1) with (ts2, v2) in
    (mini ts1 ts2, divf (addf v1 v2) 2.0)
  else
    get obs (divi n 2)
in

let emptyBuffers = lam.
  { leftDists = toList []
  , rightDists = toList []
  , leftSpeeds = toList []
  , rightSpeeds = toList [] }
in

let state = {
  frontPriorTsv = (negi 1, Uniform 0.0 4.0),
  buffers = emptyBuffers ()
} in

let n = 10 in

loopFn state (lam i. lam state.
  -- Skip the delay if we are in replay mode, when we're debugging the code.
  (if options.replaying then ()
  else sleepMs 100);

  match state with {frontPriorTsv = (lastTs, prior), buffers = buffers} in

  let frontLeft = readFloatData distanceFrontLeft in
  let frontRight = readFloatData distanceFrontRight in
  let speedLeft = readFloatData speedValLeft in
  let speedRight = readFloatData speedValRight in

  let buffers = {buffers with
    leftDists = snoc buffers.leftDists frontLeft,
    rightDists = snoc buffers.rightDists frontRight,
    leftSpeeds = snoc buffers.leftSpeeds speedLeft,
    rightSpeeds = snoc buffers.rightSpeeds speedRight
  } in

  -- Only run the model once every n iterations (every n/10 s), using the values
  -- seen since the last inference to base the observations on.
  if eqi (modi i n) 0 then

    -- TODO: how do we get the actual "current" timestamp? This will only work
    -- in replay mode, when we're never missing any data.
    let ts =
      let timestamps =
        map
          (lam x. x.0)
          (join [buffers.leftDists, buffers.rightDists, buffers.leftSpeeds, buffers.rightSpeeds])
      in
      foldl maxi (head timestamps) (tail timestamps)
    in

    let ld = median buffers.leftDists in
    let rd = median buffers.rightDists in
    let ls = median buffers.leftSpeeds in
    let rs = median buffers.rightSpeeds in

    -- Time since the last timestamp in seconds
    let deltaTime =
      if eqi lastTs (negi 1) then None ()
      else Some (divf (int2float (subi ts lastTs)) 1000.0)
    in

    -- Assume no rotation - the speed of the car is just the average of the
    -- medians reported by each of the wheels.
    let speed = divf (addf ls.1 rs.1) 2.0 in

    let posterior = infer (BPF {particles = 1000}) (lam. distanceModel prior deltaTime ld rd speed) in
    let distanceFrontTsv = (ts, posterior) in

    writeData obsDistanceFront distanceFrontTsv;

    {state with frontPriorTsv = distanceFrontTsv, buffers = emptyBuffers ()}
  else
    {state with frontPriorTsv = (lastTs, prior), buffers = buffers}
)
