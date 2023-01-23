include "math.mc"
include "set.mc"
include "ext/dist-ext.mc"

include "argparse.mc"
include "constants.mc"
include "init-pos.mc"
include "shared.mc"
include "room.mc"

let neginf = negf inf

type SensorData = (Float -> Pos -> Float, FloatTsv, Float, Float)

-- Compute difference between timestamps and convert difference to a time
-- in seconds.
let deltaT : Int -> Int -> Float = lam t0. lam t1.
  if eqi t0 0 then 0.0
  else divf (int2float (subi t1 t0)) 1000000000.0

let estimatePositionAt : (Float, Float) -> Float -> Float -> Int -> Int -> (Float, Float) =
  lam initialPos. lam speed. lam angle. lam t0. lam t1.
  match initialPos with (x0, y0) in

  -- Estimate the distance travelled based on the estimated speed and the time
  -- difference. Given the angle in which the car is travelling, we use this to
  -- estimate the new position of the car.
  let distEst = mulf speed (deltaT t0 t1) in
  let distForward = assume (Gaussian distEst (mulf distEst 0.1)) in
  ( addf x0 (mulf distForward (cos angle))
  , addf y0 (mulf distForward (sin angle)) )

let transitionModel : Bool -> Pos -> Float -> Float -> Int -> Int
                   -> (Pos, Float) =
  lam isFirstEstimation. lam pos. lam speed. lam angle. lam t0. lam t1.

  -- We do not apply the transition model in our first estimation. We assume
  -- the car is standing still at this point.
  if isFirstEstimation then (pos, angle)
  else

    -- There is some degree of uncertainty in what the actual speed and direction
    -- are.
    let newSpeed = assume (Gaussian speed 0.025) in
    let newAngle = assume (Gaussian angle (divf pi 4.0)) in

    -- Estimate the current position given speed and time difference, with some
    -- uncertainty.
    let newPos = estimatePositionAt pos speed newAngle t0 t1 in

    (newPos, newAngle)

let observeSensor : Float -> Pos -> SensorData -> Option Float =
  lam angle. lam pos. lam sensorData.

  match sensorData with (expectedDistance, (_, distObs), maxSensorRange, stdev) in
  let distEst = expectedDistance angle pos in

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
  else if ltf distEst maxSensorRange then
    Some neginf
  else None ()

let observeSensors : Float -> Pos -> [SensorData] -> Float =
  lam angle. lam pos. lam sensors.
  let weights =
    foldl
      (lam acc. lam sensor.
        match observeSensor angle pos sensor with Some w then cons w acc
        else acc)
      [] sensors
  in
  match max cmpFloat weights with Some maxw then
    if eqf maxw neginf then neginf
    else foldl addf 0.0 weights
  else error "No sensors were provided to observeSensors"

-- Compute the likelihood of making the provided observations at the
-- estimated position of the car at their respective timestamps. The
-- offsets of the sensors are taken into account when making these
-- observations, as we seek to estimate the position relative to a
-- central point of the car.
let observationModel : RoomMap -> Bool -> Float -> Pos -> [FloatTsv]
                    -> (Float, Float, Float) =
  lam m. lam isFirstEstimation. lam angle. lam pos. lam observations.

  match observations
  with [frontLeft, frontRight, rearLeft, rearRight, leftSide, rightSide] in

  let sensors = [
    (expectedDistanceFront m frontLeftOfs, frontLeft, maxLongRangeSensorDist, 0.1),
    (expectedDistanceFront m frontRightOfs, frontRight, maxLongRangeSensorDist, 0.1),
    (expectedDistanceRear m rearLeftOfs, rearLeft, maxLongRangeSensorDist, 0.1),
    (expectedDistanceRear m rearRightOfs, rearRight, maxLongRangeSensorDist, 0.1),
    (expectedDistanceLeft m leftOfs, leftSide, maxShortRangeSensorDist, 0.05),
    (expectedDistanceRight m rightOfs, rightSide, maxShortRangeSensorDist, 0.05)
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
    let angles = create 4 (lam i. mulf (int2float i) step) in
    let weights = map (lam angle. (angle, observeSensors angle pos sensors)) angles in
    match maxOrElse (lam. never) cmpWeights weights with (maxAngle, w) in
    weight w;
    (pos.0, pos.1, maxAngle)
  else
    weight (observeSensors angle pos sensors);
    (pos.0, pos.1, angle)

let positionModel : RoomMap -> Bool -> Int -> Int -> Dist (Float, Float, Float)
                 -> Float -> FloatTsv -> FloatTsv -> FloatTsv -> FloatTsv
                 -> FloatTsv -> FloatTsv -> (Float, Float, Float) =
  lam m. lam isFirstEstimation. lam t0. lam t1. lam posPrior. lam speed.
  lam frontLeft. lam frontRight.  lam rearLeft. lam rearRight. lam leftSide.
  lam rightSide.
  -- TODO: make use of the steering angles

  -- Get the previously estimated position of the car, including its direction.
  match assume posPrior with (x0, y0, direction) in
  let coord = (x0, y0) in

  match transitionModel isFirstEstimation coord speed direction t0 t1
  with (coord, direction) in

  let distanceObs =
    [frontLeft, frontRight, rearLeft, rearRight, leftSide, rightSide]
  in

  -- Check whether the estimated new position is within bounds. If it is not,
  -- we could not have moved there, so we weight with negative infinity.
  if withinRoomBounds m coord then
    let newPos = observationModel m isFirstEstimation direction coord distanceObs in
    resample;
    newPos
  else
    weight neginf;
    resample;
    (coord.0, coord.1, direction)

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
  steeringAngle, startTime, trueXCoordinate, trueYCoordinate
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
  , y = toList [] }
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
writeData obsPosition state.posPriorTsv;

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
    y = snoc buffers.y trueY
  } in

  if eqi (modi i n) 0 then

    let fld = medianTsv buffers.frontLeftDists in
    let frd = medianTsv buffers.frontRightDists in
    let rld = medianTsv buffers.rearLeftDists in
    let rrd = medianTsv buffers.rearRightDists in
    let sld = medianTsv buffers.sideLeftDists in
    let srd = medianTsv buffers.sideRightDists in
    let ls = medianTsv buffers.leftSpeeds in
    let rs = medianTsv buffers.rightSpeeds in

    -- NOTE(larshum, 2022-11-14): We assume that the observed speed is constant
    -- during the whole period. It is computed as the average of the median
    -- speed of the observations of the left and right wheels.
    let speedRPM = divf (addf ls.1 rs.1) 2.0 in
    let speedObs = divf (mulf speedRPM wheelCircumference) 60.0 in

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
                speedObs fld frd rld rrd sld srd)
    in
    let endt = wallTimeMs () in
    printLn (join ["Inference time: ", float2string (divf (subf endt start) 1000.0)]);
    match expectedValuePosDist posPosterior with (x, y, angle) in
    printLn (join ["Expected value: x=", float2string x, ", y=", float2string y, ", angle=", float2string angle]);
    flushStdout ();

    -- NOTE(larshum, 2023-01-23): This code ensures that the program does not
    -- crash while recording observations due to all particles having weight 0.
    -- If we are replaying, we stop the program and save the estimations we
    -- have done so far to buffers, instead of letting it crash later.
    let posteriorTsv =
      let posPosterior =
        if distEmpiricalDegenerate posPosterior then
          if options.replaying then
            printLn "Estimated posterior has only particles with weight 0";
            saveBuffersAndExit 2;
            never
          else positionPrior
        else posPosterior
      in
      (tcurr, posPosterior)
    in

    writeData obsPosition posteriorTsv;

    {state with posPriorTsv = posteriorTsv, buffers = emptyBuffers ()}

  else {state with buffers = buffers}
)
