include "ext/rtppl-ext.mc"
include "common.mc"
include "option.mc"
include "string.mc"

include "argparse.mc"
include "buffers.mc"
include "constants.mc"
include "shared.mc"
include "sm_conf.mc"

let maxDist = 4.0
let priorDist = Uniform 0.0 maxDist

let distAtTime = lam dist. lam speedMetersPerSec. lam t0. lam t1.
  if eqi t0 (negi 1) then
    dist
  else
    let deltaTimeSec = divf (int2float (subi t1 t0)) 1000000000.0 in
    addf dist (mulf (negf speedMetersPerSec) deltaTimeSec)

let distanceModel : (Int, Dist Float) -> Int -> (Int, Float)
                 -> (Int, Float) -> Float -> Float =
  lam priorTsv. lam t1. lam ld. lam rd. lam speedRotAvg.

  match priorTsv with (t0, prior) in

  let distance = assume prior in

  -- These are the medians of all distances observed since the last inference
  -- run.
  match ld with (lts, ldist) in
  match rd with (rts, rdist) in

  let speedMetersPerSecAvg = divf (mulf speedRotAvg wheelCircumference) 60.0 in
  let speedMetersPerSec = assume (Gaussian speedMetersPerSecAvg 0.001) in

  -- The distance is invalid if it is outside of the bounds.
  if or (ltf distance 0.0) (gtf distance maxDist) then
    weight (negf inf);
    distance
  else
    -- Observation model
    let lExpectedDist = distAtTime distance speedMetersPerSec t0 lts in
    let rExpectedDist = distAtTime distance speedMetersPerSec t0 rts in
    observe ldist (Gaussian lExpectedDist 0.01);
    observe rdist (Gaussian rExpectedDist 0.01);

    -- Compute the new distance by multiplying the estimated speed with the
    -- delta time. Note that the front distance increases when the speed is
    -- negative (when we're driving in reverse).
    distAtTime distance speedMetersPerSec t0 t1

mexpr

let options = parseOptions (tail argv) in
handleOptions options;

-- User has to declare the input and output sensors explicitly.
let inputs = [
  distanceFrontLeft, distanceFrontRight, distanceBackLeft, distanceBackRight,
  speedValLeft, speedValRight, startTime] in
let outputs = [obsDistanceFront, obsDistanceBack] in
initBuffers options inputs outputs;

let emptyBuffers = lam.
  { frontLeftDists = toList []
  , frontRightDists = toList []
  , rearLeftDists = toList []
  , rearRightDists = toList []
  , leftSpeeds = toList []
  , rightSpeeds = toList [] }
in

let state = {
  frontPriorTsv = (negi 1, Uniform 0.0 4.0),
  rearPriorTsv = (negi 1, Uniform 0.0 4.0),
  buffers = emptyBuffers ()
} in

let n = 10 in

-- We use t0 as the initial time at which execution started.
match readFloatData startTime with (t0, _) in

loopFn state (lam i. lam state.
  -- Skip the delay if we are in replay mode, when we're debugging the code.
  (if options.replaying then ()
  else sleepMs 100);

  match state with {
    frontPriorTsv = (prevFrontTs, _),
    rearPriorTsv = (prevRearTs, _),
    buffers = buffers
  } in

  let frontLeft = readFloatData distanceFrontLeft in
  let frontRight = readFloatData distanceFrontRight in
  let rearLeft = readFloatData distanceBackLeft in
  let rearRight = readFloatData distanceBackRight in
  let speedLeft = readFloatData speedValLeft in
  let speedRight = readFloatData speedValRight in

  let buffers = {buffers with
    frontLeftDists = snoc buffers.frontLeftDists frontLeft,
    frontRightDists = snoc buffers.frontRightDists frontRight,
    rearLeftDists = snoc buffers.rearLeftDists rearLeft,
    rearRightDists = snoc buffers.rearRightDists rearRight,
    leftSpeeds = snoc buffers.leftSpeeds speedLeft,
    rightSpeeds = snoc buffers.rightSpeeds speedRight
  } in

  -- Only run the model once every n iterations (every n/10 s), using the values
  -- seen since the last inference to base the observations on.
  if eqi (modi i n) 0 then

    let ts =
      if options.replaying then
        -- NOTE: This is only the correct choice for our manufactured data -
        -- not sure what it should be when replaying "actual" data?
        muli (subi i 1) 100000000
      else
        match clockGetTime () with (s, ns) in
        subi (addi (muli s 1000000000) ns) t0
    in

    let fld = median buffers.frontLeftDists in
    let frd = median buffers.frontRightDists in
    let rld = median buffers.rearLeftDists in
    let rrd = median buffers.rearRightDists in
    let ls = median buffers.leftSpeeds in
    let rs = median buffers.rightSpeeds in

    -- Assume no rotation - the speed of the car is just the average of the
    -- medians reported by each of the wheels.
    let speed = divf (addf ls.1 rs.1) 2.0 in

    -- Compute the rear and front distance using the same model, but with
    -- opposite speeds.
    let frontDistancePosterior =
      infer (BPF {particles = 1000})
        (lam. distanceModel state.frontPriorTsv ts fld frd speed)
    in
    let rearDistancePosterior =
      infer (BPF {particles = 1000})
        (lam. distanceModel state.rearPriorTsv ts rld rrd (negf speed))
    in

    let frontTsv = (ts, frontDistancePosterior) in
    let rearTsv = (ts, rearDistancePosterior) in

    writeData obsDistanceFront frontTsv;
    writeData obsDistanceBack rearTsv;

    {state with frontPriorTsv = frontTsv, rearPriorTsv = rearTsv, buffers = emptyBuffers ()}
  else {state with buffers = buffers}
)
