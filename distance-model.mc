-- Example model for the front distance estimation.
--
-- We assume the distance can be anywhere between 0 and 4 meters. The input
-- signals come from two separate sensors, with different properties.
--
-- The first sensor is accurate on short range, but loses precision when the
-- distance is above 2 meters. The second tensor is less accurate overall, but
-- can measure up to 4 meters.

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
  else if gtf distance 2.0 then
    observe ldist (Gaussian distance 0.01)
  else
    observe ldist (Gaussian distance 0.01);
    observe rdist (Gaussian distance 0.01));

  distance
