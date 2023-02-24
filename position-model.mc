include "math.mc"
include "set.mc"
include "ext/dist-ext.mc"

include "argparse.mc"
include "constants.mc"
include "init-pos.mc"
include "shared.mc"
include "room.mc"

let neginf = negf inf

type SensorData = (State -> Float, FloatTsv, Float, Float)

type DistanceObs = {
  frontLeft : FloatTsv,
  frontRight : FloatTsv,
  rearLeft : FloatTsv,
  rearRight : FloatTsv,
  sideLeft : FloatTsv,
  sideRight : FloatTsv
}

type WheelObs = [{
  t : Int,
  speed : Float,
  steeringAngle : Float
}]

let tan = lam rad. divf (sin rad) (cos rad)

-- Compute difference between timestamps and convert difference to a time
-- in seconds.
let deltaT : Int -> Int -> Float = lam t0. lam t1.
  divf (int2float (subi t1 t0)) 1000000000.0

let transitionModel : Bool -> WheelObs -> (Int, Float, Float, Float) -> Int
                   -> [(Int, Float, Float, Float)] =
  lam isFirstEstimation. lam wheelObs. lam position. lam t1.

  -- We do not apply the transition model in our first estimation. We assume
  -- the car is standing still at this point.
  if isFirstEstimation then [position]
  else
    let f = lam pos. lam obs.
      match pos with (t0, x0, y0, direction) in
      match obs with {t = t1, speed = speed, steeringAngle = steeringAngle} in

      -- We assume there is a small amount of uncertainty in the observed speed
      -- and steering angle.
      let newSpeed = assume (Gaussian speed 0.01) in
      let steeringAngle = assume (Gaussian steeringAngle 0.05) in

      -- Estimate the new direction the car is facing based on the estimates.
      let distTravelled = mulf newSpeed (deltaT t0 t1) in
      let directionDelta = divf (mulf distTravelled (tan steeringAngle)) 0.45 in
      let newDirection = addf direction directionDelta in

      -- Estimate the new x- and y-coordinates based on the previous
      -- estimations.
      let x1 = addf x0 (mulf distTravelled (cos newDirection)) in
      let y1 = addf y0 (mulf distTravelled (sin newDirection)) in
      let newPos = (t1, x1, y1, newDirection) in
      (newPos, newPos)
    in
    match mapAccumL f position wheelObs with (_, positions) in
    positions

let findClosestPositionInTime : [(Int, Float, Float, Float)] -> Int
                             -> State =
  lam positions. lam t.
  let absDiff = lam p1. lam p2.
    let d1 = absi (subi p1.0 t) in
    let d2 = absi (subi p2.0 t) in
    if lti d1 d2 then 1
    else if gti d1 d2 then negi 1
    else 0
  in
  match min absDiff positions with Some (_, x, y, direction) then
    {x = x, y = y, direction = direction}
  else error "Transition model made no position estimations"

let observeSensor : [(Int, Float, Float, Float)] -> SensorData -> Option Float =
  lam positions. lam sensorData.

  match sensorData with (expectedDistance, (t, distObs), maxSensorRange, stdev) in

  -- NOTE(larshum, 2023-01-24): Choose the intermediate position whose
  -- timestamp is the closest to that of the observation. This should result in
  -- a more accurate estimation of likelihood, in particular when the car is
  -- moving around.
  let state = findClosestPositionInTime positions t in

  let distEst = expectedDistance state in

  -- NOTE(larshum, 2023-01-13): We handle three cases for each sensor
  -- * If the observed distance is within its bounds, we consider how likely
  --   the observation is given an estimated distance based on the position.
  -- * Otherwise, it greater than the max distance. If the estimated distance
  --   is less than this, the car cannot be at the current position.
  -- * Otherwise, both the observed and estimated distances are greater than
  --   the max distance. In this case, we do not get any information from the
  --   sensor.
  if ltf distObs maxSensorRange then
    Some (gaussianLogPdf distEst stdev distObs)
  else if ltf distEst (mulf 0.9 maxSensorRange) then
    Some neginf
  else None ()

let observeSensors : [(Int, Float, Float, Float)] -> [SensorData] -> Float =
  lam positions. lam sensors.
  let weights =
    foldl
      (lam acc. lam sensor.
        match observeSensor positions sensor with Some w then
          cons w acc
        else acc)
      [] sensors
  in
  match max cmpFloat weights with Some maxw then
    if eqf maxw neginf then neginf
    else foldl addf 0.0 weights
  else 0.0

-- Compute the likelihood of making the provided observations at the
-- estimated position of the car at their respective timestamps. The
-- offsets of the sensors are taken into account when making these
-- observations, as we seek to estimate the position relative to a
-- central point of the car.
let observationModel : RoomMap -> Bool -> [(Int, Float, Float, Float)] -> DistanceObs
                    -> State =
  lam m. lam isFirstEstimation. lam positions. lam distanceObs.

  match last positions with (_, x, y, direction) in
  let pos = (x, y) in

  let sensors = [
    (expectedDistanceFront m frontLeftOfs, distanceObs.frontLeft, maxLongRangeSensorDist, 0.1),
    (expectedDistanceFront m frontRightOfs, distanceObs.frontRight, maxLongRangeSensorDist, 0.1),
    (expectedDistanceRear m rearLeftOfs, distanceObs.rearLeft, maxLongRangeSensorDist, 0.1),
    (expectedDistanceRear m rearRightOfs, distanceObs.rearRight, maxLongRangeSensorDist, 0.1),
    (expectedDistanceLeft m leftOfs, distanceObs.sideLeft, maxShortRangeSensorDist, 0.05),
    (expectedDistanceRight m rightOfs, distanceObs.sideRight, maxShortRangeSensorDist, 0.05)
  ] in
  if isFirstEstimation then
    let cmpWeights = lam l. lam r.
      match (l, r) with ((_, lw), (_, rw)) in
      let d = subf lw rw in
      if ltf d 0.0 then negi 1
      else if gtf d 0.0 then 1
      else 0
    in
    let step = divf pi 2.0 in
    let directions = create 4 (lam i. mulf (int2float i) step) in
    let weights =
      map
        (lam direction.
          let positions = map (lam p. {p with #label"3" = direction}) positions in
          (direction, observeSensors positions sensors))
        directions
    in
    match maxOrElse (lam. never) cmpWeights weights with (maxDirection, w) in
    weight w;
    {x = x, y = y, direction = maxDirection}
  else
    weight (observeSensors positions sensors);
    {x = x, y = y, direction = direction}

let positionModel : RoomMap -> Bool -> Int -> Int -> Dist State -> DistanceObs
                 -> WheelObs -> State =
  lam m. lam isFirstEstimation. lam t0. lam t1. lam posPrior. lam distanceObs.
  lam wheelObs.

  -- Get the previously estimated position of the car, including its direction.
  match assume posPrior with {x = x, y = y, direction = direction} in
  let prevPosition = (t0, x, y, direction) in

  let positions = transitionModel isFirstEstimation wheelObs prevPosition t1 in

  -- Verify that all intermediate positions are within the bounds of the map.
  -- If any intermediate position is not, it means the car would have moved
  -- into a wall. As this is impossible, we weight with minus infinity in such
  -- cases.
  let coords = map (lam p. (p.1, p.2)) positions in
  let newPos =
    if forAll (withinRoomBounds m) coords then
      observationModel m isFirstEstimation positions distanceObs
    else
      weight neginf;
      match last positions with (_, x, y, direction) in
      {x = x, y = y, direction = direction}
  in
  resample;
  newPos

mexpr

let options = parseOptions (tail argv) in
handleOptions options;

let roomMap =
  if null options.roomMapFile then
    error "A file encoding the room must be provided using the --room-map option"
  else readMap options.roomMapFile
in

let inputs = [
  distanceFrontLeft, distanceFrontRight, distanceBackLeft, distanceBackRight,
  distanceSideLeft, distanceSideRight, speedValLeft, speedValRight,
  steeringAngle, startTime, trueXCoordinate, trueYCoordinate, trueDirection
] in
let outputs = [obsPosition] in
let options = {options with bufferOnlyOutputs = setOfSeq subi [obsPosition]} in
initBuffers options inputs outputs;

let emptyBuffers = lam.
  { frontLeftDists = toList []
  , frontRightDists = toList []
  , rearLeftDists = toList []
  , rearRightDists = toList []
  , sideLeftDists = toList []
  , sideRightDists = toList []
  , leftSpeeds = toList []
  , rightSpeeds = toList []
  , steeringAngles = toList []
  , x = toList []
  , y = toList []
  , d = toList [] }
in

match readFloatData startTime with (t0, _) in

-- We get the maximum x- and y-values within the map by converting its
-- dimensions to a position.
match coordToPosition (roomDims roomMap) with (maxX, maxY) in
printLn (join ["Room dimensions: ", float2string maxX, " ", float2string maxY]);

-- Configuration parameters
let n = 9 in
let period = 100 in

-- NOTE(larshum, 2023-01-12): We define the distribution for the initial
-- position in 'init-pos.mc'.
let positionPrior = initPosDist roomMap in
let state = {
  posPriorTsv = (t0, positionPrior),
  buffers = emptyBuffers ()
} in
writeData obsPosition (unsafeCoerce state.posPriorTsv);

loopFn state (lam i. lam state.
  -- Skip the delay if we are in replay mode
  (if options.replaying then 0
  else sdelay 100);

  match state with {
    posPriorTsv = (tprev, posPrior),
    buffers = buffers
  } in

  let frontLeft = readFloatData distanceFrontLeft in
  let frontRight = readFloatData distanceFrontRight in
  let rearLeft = readFloatData distanceBackLeft in
  let rearRight = readFloatData distanceBackRight in
  let sideLeft = readFloatData distanceSideLeft in
  let sideRight = readFloatData distanceSideRight in
  let speedLeft = readFloatData speedValLeft in
  let speedRight = readFloatData speedValRight in
  let angle = readFloatData steeringAngle in

  -- NOTE(larshum, 2023-01-20): We do not make use of these in the model. They
  -- are only used in a postprocessing step.
  let trueX = readFloatData trueXCoordinate in
  let trueY = readFloatData trueYCoordinate in
  let trueD = readFloatData trueDirection in

  let buffers = {buffers with
    frontLeftDists = snoc buffers.frontLeftDists frontLeft,
    frontRightDists = snoc buffers.frontRightDists frontRight,
    rearLeftDists = snoc buffers.rearLeftDists rearLeft,
    rearRightDists = snoc buffers.rearRightDists rearRight,
    sideLeftDists = snoc buffers.sideLeftDists sideLeft,
    sideRightDists = snoc buffers.sideRightDists sideRight,
    leftSpeeds = snoc buffers.leftSpeeds speedLeft,
    rightSpeeds = snoc buffers.rightSpeeds speedRight,
    steeringAngles = snoc buffers.steeringAngles angle,
    x = snoc buffers.x trueX,
    y = snoc buffers.y trueY,
    d = snoc buffers.d trueD
  } in

  if eqi (modi i n) 0 then

    let distanceObs = {
      frontLeft = medianTsv buffers.frontLeftDists,
      frontRight = medianTsv buffers.frontRightDists,
      rearLeft = medianTsv buffers.rearLeftDists,
      rearRight = medianTsv buffers.rearRightDists,
      sideLeft = medianTsv buffers.sideLeftDists,
      sideRight = medianTsv buffers.sideRightDists
    } in
    let wheelObs =
      -- NOTE(larshum, 2023-01-24): Wheel observations are made at roughly the
      -- same point in time, but they may have slight differences. We choose to
      -- use the maximum out of the three as the combined timestamp.
      create (length buffers.leftSpeeds)
        (lam i.
          match ( get buffers.leftSpeeds i, get buffers.rightSpeeds i
                , get buffers.steeringAngles i )
          with ((t1, ls), (t2, rs), (t3, sa)) in
          let speedRPM = divf (addf ls rs) 2.0 in
          let speedMs = divf (mulf speedRPM wheelCircumference) 60.0 in
          {t = maxi (maxi t1 t2) t3, speed = speedMs, steeringAngle = negf (degToRad sa)})
    in

    -- Compute the next timestamp based on the timestamp of the prior
    -- estimation and the period.
    let tcurr = addi tprev (muli n (muli period 1000000)) in

    let start = wallTimeMs () in
    -- NOTE(larshum, 2023-01-23): If the previous timestamp is equal to the
    -- initial timestamp we found, this is the first estimation. In the first
    -- estimation, we use a different approach from later ones.
    let isFirstEstimation = eqi t0 tprev in
    let posPosterior =
      infer (BPF {particles = 10000})
        (lam. positionModel roomMap isFirstEstimation tprev tcurr posPrior
                            distanceObs wheelObs)
    in
    let endt = wallTimeMs () in
    printLn (join ["Inference time: ", float2string (divf (subf endt start) 1000.0)]);
    match expectedValuePosDist posPosterior with {x = x, y = y, direction = d} in
    printLn (join ["T : ", int2string tcurr]);
    printLn (join ["Expected value: x=", float2string x, ", y=", float2string y, ", angle=", float2string d]);
    printLn (join ["True value    : x=", float2string (last buffers.x).1,
                   ", y=", float2string (last buffers.y).1,
                   ", angle=", float2string (last buffers.d).1]);
    flushStdout ();

    -- NOTE(larshum, 2023-01-23): This code ensures that the program does not
    -- crash while recording observations due to all particles having weight 0.
    -- If we are replaying, we stop the program and save the estimations we
    -- have done so far to buffers, instead of letting it crash later.
    let posteriorTsv =
      let posPosterior =
        if distEmpiricalDegenerate posPosterior then
          if options.replaying then
            match distEmpiricalSamples posPosterior with (samples, _) in
            printLn "Estimated posterior has only particles with weight 0";
            printLn (int2string (length samples));
            iter (lam s. printLn (join [float2string s.x, " ", float2string s.y])) samples;
            saveBuffersAndExit 2;
            never
          else positionPrior
        else posPosterior
      in
      (tcurr, posPosterior)
    in

    writeData obsPosition (unsafeCoerce posteriorTsv);

    {state with posPriorTsv = posteriorTsv, buffers = emptyBuffers ()}

  else {state with buffers = buffers}
)
