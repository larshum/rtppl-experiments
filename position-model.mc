include "math.mc"
include "set.mc"
include "ext/dist-ext.mc"

include "argparse.mc"
include "constants.mc"
include "init-pos.mc"
include "print.mc"
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

type WheelObs = {
  speedLeft : [FloatTsv],
  speedRight : [FloatTsv],
  steeringAngle : [FloatTsv]
}

-- Compute difference between timestamps and convert difference to a time
-- in seconds.
let deltaT : Int -> Int -> Float = lam t0. lam t1.
  divf (int2float (subi t1 t0)) 1000000000.0

let transitionModel : Bool -> WheelObs -> State -> Int -> [State] =
  lam isFirstEstimation. lam wheelObs. lam state. lam t1.
  if isFirstEstimation then [{state with ts = t1}]
  else
    let f = lam state. lam action.
      match action with (ts, (actionType, actionValue)) in

      -- Estimate the new position of the car, assuming it drives at constant
      -- speed and steering angle from the timestamp associated with the state
      -- until the timestamp of the next action.
      let delta = deltaT state.ts ts in
      let dist = mulf state.speed delta in
      let directionDelta = divf (mulf dist (tan state.steeringAngle)) 0.45 in
      let direction = addf state.direction directionDelta in
      let state = {state with x = addf state.x (mulf dist (cos direction)),
                              y = addf state.y (mulf dist (sin direction)),
                              direction = direction, ts = ts} in

      -- Update the state of the car based on the "action" associated with the
      -- corresponding observation.
      switch actionType
      case 0 | 1 then
        let speedMs = divf (mulf actionValue wheelCircumference) 60.0 in
        {state with speed = assume (Gaussian speedMs 0.01)}
      case 2 then
        let angle = negf (degToRad actionValue) in
        {state with steeringAngle = assume (Gaussian angle 0.05)}
      case 3 then
        state
      end
    in
    let sl = map (lam t. (t.0, (0, t.1))) wheelObs.speedLeft in
    let sr = map (lam t. (t.0, (1, t.1))) wheelObs.speedRight in
    let sa = map (lam t. (t.0, (2, t.1))) wheelObs.steeringAngle in
    let actions = snoc (sort cmpTsvTime (join [sl, sr, sa])) (t1, (3, 0.0)) in
    foldl
      (lam states. lam action. snoc states (f (last states) action))
      [state] actions

let findClosestStateInTime : [State] -> Int -> State =
 lam states. lam t.
 let absDiff = lam p1. lam p2.
    let d1 = absi (subi p1.ts t) in
    let d2 = absi (subi p2.ts t) in
    if lti d1 d2 then 1
    else if gti d1 d2 then negi 1
    else 0
 in
 match min absDiff states with Some state then
   state
 else
   error "Transition model made no position estimations"

let observeSensor : [State] -> SensorData -> Option Float =
  lam states. lam sensorData.

  match sensorData with (expectedDistance, (t, distObs), maxSensorRange, stdev) in

  -- NOTE(larshum, 2023-01-24): Choose the intermediate state whose timestamp
  -- is the closest to that of the observation. This should result in a more
  -- accurate estimation of likelihood with less computational requirements.
  let state = findClosestStateInTime states t in
  let distEst = expectedDistance state in

  if ltf distObs maxSensorRange then
    Some (gaussianLogPdf distEst stdev distObs)
  else None ()

let observeSensors : [State] -> [SensorData] -> Float =
  lam states. lam sensors.
  let weights =
    foldl
      (lam acc. lam sensor.
        match observeSensor states sensor with Some w then
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
let observationModel : RoomMap -> Bool -> [State] -> DistanceObs -> State =
  lam m. lam isFirstEstimation. lam states. lam distanceObs.

  let s = last states in

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
          let states = map (lam s. {s with direction = direction}) states in
          (direction, observeSensors states sensors))
        directions
    in
    match maxOrElse (lam. never) cmpWeights weights with (maxDirection, w) in
    weight w;
    {s with direction = maxDirection}
  else
    weight (observeSensors states sensors);
    s

let positionModel : RoomMap -> Bool -> Int -> Dist State -> DistanceObs
                 -> WheelObs -> State =
  lam m. lam isFirstEstimation. lam t1. lam prior. lam distanceObs.
  lam wheelObs.

  let state = assume prior in
  let states = transitionModel isFirstEstimation wheelObs state t1 in

  -- Verify that all intermediate positions are within the bounds of the map.
  -- If any intermediate position is not, it means the car would have moved
  -- into a wall. As this is impossible, we weight with minus infinity in such
  -- cases.
  let coords = map (lam s. (s.x, s.y)) states in
  let state =
    if forAll (withinRoomBounds m) coords then
      observationModel m isFirstEstimation states distanceObs
    else
      weight neginf;
      last states
  in
  resample;
  state

mexpr

let options = parseOptions (tail argv) in
handleOptions options;

let roomMap =
  if null options.roomMapFile then
    error "A file encoding the room must be provided using the --room-map option"
  else readMap options.roomMapFile
in

let t0 =
  if options.replaying then 0
  else
    match clockGetTime () with (s, ns) in
    addi ns (muli 1000000000 s)
in

let inputs = [
  distanceFrontLeft, distanceFrontRight, distanceBackLeft, distanceBackRight,
  distanceSideLeft, distanceSideRight, speedValLeft, speedValRight,
  steeringAngle, startTime, trueXCoordinate, trueYCoordinate, trueDirection
] in
let outputs = [obsPosition] in
let options = {options with bufferOnlyOutputs = setOfSeq subi [obsPosition]} in
initBuffers options t0 inputs outputs;

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

-- We get the maximum x- and y-values within the map by converting its
-- dimensions to a position.
match coordToPosition (roomDims roomMap) with (maxX, maxY) in
printLn (join ["Room dimensions: ", float2string maxX, " ", float2string maxY]);

-- Configuration parameters
let n = 9 in
let period = 100 in

-- NOTE(larshum, 2023-01-12): We define the distribution for the initial
-- position in 'init-pos.mc'.
let positionPrior = initPosDist t0 roomMap in
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
    let wheelObs = {
      speedLeft = buffers.leftSpeeds,
      speedRight = buffers.rightSpeeds,
      steeringAngle = buffers.steeringAngles
    } in

    -- Compute the next timestamp based on the timestamp of the prior
    -- estimation and the period.
    let tcurr = addi tprev (muli n (muli period 1000000)) in

    let start = wallTimeMs () in
    let isFirstEstimation = eqi t0 tprev in
    let posPosterior =
      infer (BPF {particles = 1000})
        (lam. positionModel roomMap isFirstEstimation tcurr posPrior
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
