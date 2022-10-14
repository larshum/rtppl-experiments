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

let distanceModel = lam prior. lam lobs. lam robs.
  let distance = assume prior in

  (if or (ltf distance 0.0) (gtf distance maxDist) then
    weight (negf inf)
  else if gtf dist 2.0 then
    observe lobs (Normal distance 0.01)
  else
    observe lobs (Normal distance 0.01);
    observe robs (Normal distance 0.01));

  distance
