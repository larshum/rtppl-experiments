-- Example model for the front distance estimation.

include "math.mc"

let maxDist = 4.0
let priorDist = Uniform 0.0 maxDist

let distanceModel : Dist Float -> (Int, Float) -> (Int, Float) -> Float =
  lam prior. lam lobs. lam robs.
  let distance = assume prior in

  -- For now, the model ignores the timestamps
  match lobs with (_, ldist) in
  match robs with (_, rdist) in

  (if or (ltf distance 0.0) (gtf distance maxDist) then
    weight (negf inf)
  else
    observe ldist (Gaussian distance 0.01);
    observe rdist (Gaussian distance 0.01));

  distance
