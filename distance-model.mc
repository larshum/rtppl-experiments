-- Example model for the front distance estimation.

include "math.mc"

let maxDist = 4.0
let priorDist = Uniform 0.0 maxDist

let wheelCircumference = 0.35

let distanceModel : Dist Float -> Option Float -> (Int, Float) -> (Int, Float)
                 -> Float -> Float =
  lam prior. lam deltaT. lam ld. lam rd. lam speedRotAvg.
  let distance = assume prior in

  -- These are the medians of all distances observed since the last inference
  -- run.
  match ld with (_, ldist) in
  match rd with (_, rdist) in

  let speedMsAvg = mulf speedRotAvg wheelCircumference in
  let speedMs = assume (Gaussian speedMsAvg 0.001) in

  if or (ltf distance 0.0) (gtf distance maxDist) then
    weight (negf inf);
    distance
  else
    -- Observation model
    observe ldist (Gaussian distance 0.01);
    observe rdist (Gaussian distance 0.01);

    -- Transition model. Unless this is the first iteration, we have some time
    -- difference from the previous iteration (dt > 0).
    let dt = optionGetOrElse (lam. 0.0) deltaT in

    -- Compute the new distance by multiplying the estimated speed with the
    -- delta time. Note that the front distance increases when the speed is
    -- negative (when we're driving in reverse).
    addf distance (mulf (negf speedMs) dt)
