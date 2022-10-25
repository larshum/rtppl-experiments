-- Example model for the front distance estimation.

include "math.mc"

let maxDist = 4.0
let priorDist = Uniform 0.0 maxDist

let wheelCircumference = 0.35

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

  let speedMetersPerSecAvg = mulf speedRotAvg wheelCircumference in
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
