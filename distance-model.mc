-- Example model for the front distance estimation.

include "math.mc"

let maxDist = 4.0
let priorDist = Uniform 0.0 maxDist

let wheelCircumference = 0.35

let distanceModel : Dist Float -> Option Float -> (Int, Float) -> (Int, Float)
                 -> Float -> Float =
  lam prior. lam deltaT. lam ld. lam rd. lam speedRotAvg.
  let distance = assume prior in

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

    -- Transition model
    match deltaT with Some dt then
      let distancePrime = addf distance (mulf (negf speedMs) dt) in
      observe distancePrime (Gaussian distance 0.01);
      distancePrime
    else
      distance
