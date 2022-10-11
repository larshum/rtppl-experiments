-- Example model for the front distance estimation.
--
-- We assume the distance can be anywhere between 0 and 4 meters. The input
-- signals come from two separate sensors, with different properties.
--
-- The first sensor is accurate on short range, but loses precision when the
-- distance is above 2 meters. The second tensor is less accurate overall, but
-- can measure up to 4 meters.

let maxDist = 4.0 in

let priorDist = Uniform 0.0 maxDist in

loop priorDist (lam prior.
  -- Run every 20 ms
  let ovr = sdelay 20 in

  -- Read time-stamped values from the two sensors described above. The tuple
  -- consists of an integer time-stamp and a floating-point value (in this
  -- case, the observed distance).
  match lvRead front1 with (ts1, distObs1) in
  match lvRead front2 with (ts2, distObs2) in

  let posterior = infer (lam.
    -- Assume the distance is a value from the prior distribution.
    let dist = assume prior in

    -- We assume distances beyond 4 meters or less than 0 meters are
    -- impossible. In this case we weight by -inf (to ignore the estimate).
    --
    -- If the estimated distance is above 2 meters, we only care about the
    -- observation from the second sensor, so we do not include the 'observe'
    -- for the first one. This model assumes the accuracy of the sensors
    -- is negatively proportional to the distance.
    --
    -- TODO: This should probably be a more smooth function, which puts less
    -- weight on the first sensor for longer distances.
    -- TODO: Measure what the actual distribution of the sensor observations
    -- are.
    (if or (gtf dist maxDist) (ltf dist 0.0) then
      weight neginf
    else if gtf dist 2.0 then
      observe distObs2 (Normal dist (mulf 0.2 dist))
    else
      observe distObs1 (Normal dist (mulf 0.1 dist));
      observe distObs2 (Normal dist (mulf 0.2 dist))
    );

    dist) in

  -- Write the encoded posterior distribution to memory. We use the least
  -- recent timestamp to go along with this.
  let ts3 = min3 ts1 ts2 in
  lvWrite distance (ts3, posterior);

  -- Return the posterior of the model. This is provided as the prior to the
  -- next "iteration" of the loop.
  posterior
)
