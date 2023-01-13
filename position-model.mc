include "math.mc"
include "set.mc"
include "ext/dist-ext.mc"

include "argparse.mc"
include "constants.mc"
include "init-pos.mc"
include "shared.mc"
include "room.mc"

let neginf = negf inf

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

let positionModel : RoomMap -> Int -> Dist (Float, Float, Float) -> Int -> Float -> FloatTsv
                 -> FloatTsv -> FloatTsv -> FloatTsv -> FloatTsv -> FloatTsv
                 -> [FloatTsv] -> (Float, Float, Float) =
  lam m. lam t0. lam posPrior. lam t1. lam speed. lam frontLeft. lam frontRight.
  lam rearLeft. lam rearRight. lam leftSide. lam rightSide. lam steeringAngles.

  -- Get an estimate of the previous position of the car.
  match assume posPrior with (x0, y0, angle) in
  let initPos = (x0, y0) in

  match coordToPosition (roomDims m) with (maxX, maxY) in
  let maxDist = sqrt (addf (mulf maxX maxX) (mulf maxY maxY)) in

  -- There is some degree of uncertainty in what the actual speed is.
  let speed = assume (Gaussian speed 0.025) in

  -- We compute the new angle from the observed steering angle inputs since
  -- the last estimation. The uncertainty is included in the observed
  -- steering angles.
  /-let expectedAngle =
    if eqi t0 0 then angle
    else
      (foldl
        (lam acc. lam angleObs.
          match acc with (tsPrev, angle) in
          match angleObs with (ts, steeringAngleObs) in
          let steeringAngleObs = degToRad steeringAngleObs in
          let steeringAngle = assume (Gaussian steeringAngleObs 0.01) in
          --printLn (join [" ", float2string (deltaT tsPrev ts), " ", float2string speed, " ", float2string (tan steeringAngle)]);
          let angleDelta = divf (mulf (mulf speed (deltaT tsPrev ts))
                                      (tan steeringAngle)) 0.45 in
          (ts, addf angle angleDelta))
        (t0, angle) steeringAngles).1
  in
  let newAngle = assume (Gaussian expectedAngle 0.01) in-/
  let newAngle = assume (Gaussian angle (divf pi 4.)) in

  -- Estimate the current position given speed and time difference, with some
  -- uncertainty.
  let pos = estimatePositionAt initPos speed newAngle t0 t1 in

  -- Check whether the estimated new position is within bounds. If it is not,
  -- we could not have moved there, so we weight with negative infinity.
  (if withinRoomBounds m pos then

    -- Compute the likelihood of making the provided observations at the
    -- estimated position of the car at their respective timestamps. The
    -- offsets of the sensors are taken into account when making these
    -- observations, as we seek to estimate the position relative to a
    -- central point of the car.

    -- NOTE(larshum, 2023-01-13): We handle three cases for each sensor
    -- * If the observed distance is within its bounds, we consider how likely
    --   the observation is given an estimated distance based on the position.
    -- * Otherwise, it greater than the max distance. If the estimated distance
    --   is less than this, the car cannot be at the current position.
    -- * Otherwise, both the observed and estimated distances are greater than
    --   the max distance. In this case, we do not get any information from the
    --   sensor.
    match frontLeft with (tsFl, frontLeftObs) in
    let frontLeftEst = expectedDistanceFront m newAngle pos frontLeftOfs in
    (if ltf frontLeftObs maxLongRangeSensorDist then
      observe frontLeftObs (Gaussian frontLeftEst 0.1)
    else if ltf frontLeftEst maxLongRangeSensorDist then weight neginf
    else ());

    match frontRight with (tsFr, frontRightObs) in
    let frontRightEst = expectedDistanceFront m newAngle pos frontRightOfs in
    (if ltf frontRightObs maxLongRangeSensorDist then
      observe frontRightObs (Gaussian frontRightEst 0.1)
    else if ltf frontRightEst maxLongRangeSensorDist then weight neginf
    else ());

    match rearLeft with (tsRl, rearLeftObs) in
    let rearLeftEst = expectedDistanceRear m newAngle pos rearLeftOfs in
    (if ltf rearLeftObs maxLongRangeSensorDist then
      observe rearLeftObs (Gaussian rearLeftEst 0.1)
    else if ltf rearLeftEst maxLongRangeSensorDist then weight neginf
    else ());

    match rearRight with (tsRr, rearRightObs) in
    let rearRightEst = expectedDistanceRear m newAngle pos rearRightOfs in
    (if ltf rearRightObs maxLongRangeSensorDist then
      observe rearRightObs (Gaussian rearRightEst 0.1)
    else if ltf rearRightEst maxLongRangeSensorDist then weight neginf
    else ());

    match leftSide with (tsLeft, leftSideObs) in
    let leftSideEst = expectedDistanceLeft m newAngle pos leftOfs in
    (if ltf leftSideObs maxShortRangeSensorDist then
      observe leftSideObs (Gaussian leftSideEst 0.05)
    else if ltf leftSideEst maxShortRangeSensorDist then weight neginf
    else ());

    match rightSide with (tsRight, rightSideObs) in
    let rightSideEst = expectedDistanceRight m newAngle pos rightOfs in
    (if ltf rightSideObs maxShortRangeSensorDist then
      observe rightSideObs (Gaussian rightSideEst 0.05)
    else if ltf rightSideEst maxShortRangeSensorDist then weight neginf
    else ())
  else
    weight neginf);

  resample;
  (pos.0, pos.1, newAngle)

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
  steeringAngle, startTime
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
  , steeringAngles = toList [] }
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
  posPriorTsv = (0, positionPrior),
  buffers = emptyBuffers ()
} in
writeData obsPosition state.posPriorTsv;

loopFn state (lam i. lam state.
  -- Skip the delay if we are in replay mode
  (if options.replaying then 0
  else sdelay 100);

  match state with {
    posPriorTsv = (prevTs, posPrior),
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

  let buffers = {buffers with
    frontLeftDists = snoc buffers.frontLeftDists frontLeft,
    frontRightDists = snoc buffers.frontRightDists frontRight,
    rearLeftDists = snoc buffers.rearLeftDists rearLeft,
    rearRightDists = snoc buffers.rearRightDists rearRight,
    sideLeftDists = snoc buffers.sideLeftDists sideLeft,
    sideRightDists = snoc buffers.sideRightDists sideRight,
    leftSpeeds = snoc buffers.leftSpeeds speedLeft,
    rightSpeeds = snoc buffers.rightSpeeds speedRight,
    steeringAngles = snoc buffers.steeringAngles angle
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
    let t1 = addi prevTs (muli n (muli period 1000000)) in

    let start = wallTimeMs () in
    let posPosterior =
      infer (BPF {particles = 10000})
        (lam. positionModel roomMap prevTs posPrior t1 speedObs fld frd rld
                rrd sld srd buffers.steeringAngles)
    in
    let endt = wallTimeMs () in
    printLn (join ["Inference time: ", float2string (divf (subf endt start) 1000.0)]);
    match expectedValuePosDist posPosterior with (x, y, angle) in
    printLn (join ["Expected value: x=", float2string x, ", y=", float2string y, ", angle=", float2string angle]);
    flushStdout ();

    let posteriorTsv = (t1, posPosterior) in

    writeData obsPosition posteriorTsv;

    {state with posPriorTsv = posteriorTsv, buffers = emptyBuffers ()}

  else {state with buffers = buffers}
)
