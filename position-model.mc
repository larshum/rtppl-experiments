include "math.mc"

include "argparse.mc"
include "constants.mc"
include "shared.mc"
include "room.mc"

let positionModel : RoomMap -> (Int, Dist [Float]) -> Int -> Float -> Float
                 -> Float -> Float -> Float -> Float -> Float -> [Float] =
  lam m. lam posPriorTsv. lam t1. lam speed. lam frontLeftObs. lam frontRightObs.
  lam rearLeftObs. lam rearRightObs. lam leftDistObs. lam rightDistObs.

  -- Get an estimate of the previous position of the car.
  match posPriorTsv with (t0, posPrior) in
  match assume posPrior with [x0, y0, angle] in

  -- NOTE: we assume all observations have the same timestamp
  if withinRoomBounds m (x0, y0) then

    -- NOTE: Perhaps we could improve this by considering the distance the
    -- wheels have travelled (if turning left, the right wheel should have
    -- moved a longer distance, for example).
    let newAngle = assume (Gaussian angle pi) in

    -- Compute difference between timestamps and convert difference to a time
    -- in seconds.
    let timeDiff =
      if gti t0 t1 then 0.0
      else divf (int2float (subi t1 t0)) 1000000000.0 in

    -- Estimate the current position given speed and time difference, with some
    -- uncertainty.
    let dist = assume (Gaussian (mulf speed timeDiff) 0.02) in
    let x1 = addf x0 (mulf dist (cos newAngle)) in
    let y1 = addf y0 (mulf dist (sin newAngle)) in
    let pos = (x1, y1) in

    -- Compute the likelihood of making the observations given the assumed
    -- position (including angle) of the car. As the goal of the model is to
    -- estimate the position (of the center) of the car, we take the offsets of
    -- the individual sensors into account in the model.
    (if ltf frontLeftObs maxLongRangeSensorDist then
      let frontLeftDist = expectedDistanceFront m newAngle pos frontLeftOfs in
      observe frontLeftObs (Gaussian frontLeftDist 0.02)
    else ());

    (if ltf frontRightObs maxLongRangeSensorDist then
      let frontRightDist = expectedDistanceFront m newAngle pos frontRightOfs in
      observe frontRightObs (Gaussian frontRightDist 0.02)
    else ());

    (if ltf rearLeftObs maxLongRangeSensorDist then
      let rearLeftDist = expectedDistanceRear m newAngle pos rearLeftOfs in
      observe rearLeftObs (Gaussian rearLeftDist 0.02)
    else ());

    (if ltf rearRightObs maxLongRangeSensorDist then
      let rearRightDist = expectedDistanceRear m newAngle pos rearRightOfs in
      observe rearRightObs (Gaussian rearRightDist 0.02)
    else ());

    (if ltf leftDistObs maxShortRangeSensorDist then
      let leftDist = expectedDistanceLeft m newAngle pos leftOfs in
      observe leftDistObs (Gaussian leftDist 0.01)
    else ());

    (if ltf rightDistObs maxShortRangeSensorDist then
      let rightDist = expectedDistanceRight m newAngle pos rightOfs in
      observe rightDistObs (Gaussian rightDist 0.01)
    else ());

    [x1, y1, newAngle]
  else
    weight (negf inf);
    [x0, y0, angle]

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
  distanceSideLeft, distanceSideRight, speedValLeft, speedValRight, startTime
] in
let outputs = [obsPosition] in
initBuffers options inputs outputs;

let emptyBuffers = lam.
  { frontLeftDists = toList []
  , frontRightDists = toList []
  , rearLeftDists = toList []
  , rearRightDists = toList []
  , sideLeftDists = toList []
  , sideRightDists = toList []
  , leftSpeeds = toList []
  , rightSpeeds = toList [] }
in

match readFloatData startTime with (t0, _) in

-- We get the maximum x- and y-values within the map by converting its
-- dimensions to a position.
match coordToPosition (roomDims roomMap) with (maxX, maxY) in

let positionPrior =
  distCombineIndependent
    [ Uniform 0.0 maxX -- prior for x-coordinate
    , Uniform 0.0 maxY -- prior for y-coordinate
    , Uniform 0.0 (mulf 2.0 pi) ] -- prior for the angle (direction of the car)
in
let state = {
  posPriorTsv = (t0, positionPrior),
  buffers = emptyBuffers ()
} in

let n = 10 in

loopFn state (lam i. lam state.
  -- Skip the delay if we are in replay mode
  (if options.replaying then 0
  else sdelay 100);

  match state with {
    posPriorTsv = (prevTs, _),
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

  let buffers = {buffers with
    frontLeftDists = snoc buffers.frontLeftDists frontLeft,
    frontRightDists = snoc buffers.frontRightDists frontRight,
    rearLeftDists = snoc buffers.rearLeftDists rearLeft,
    rearRightDists = snoc buffers.rearRightDists rearRight,
    sideLeftDists = snoc buffers.sideLeftDists sideLeft,
    sideRightDists = snoc buffers.sideRightDists sideRight,
    leftSpeeds = snoc buffers.leftSpeeds speedLeft,
    rightSpeeds = snoc buffers.rightSpeeds speedRight
  } in

  if eqi (modi i n) 0 then

    let fld = median cmpTsv tsvAvg buffers.frontLeftDists in
    let frd = median cmpTsv tsvAvg buffers.frontRightDists in
    let rld = median cmpTsv tsvAvg buffers.rearLeftDists in
    let rrd = median cmpTsv tsvAvg buffers.rearRightDists in
    let sld = median cmpTsv tsvAvg buffers.sideLeftDists in
    let srd = median cmpTsv tsvAvg buffers.sideRightDists in
    let ls = median cmpTsv tsvAvg buffers.leftSpeeds in
    let rs = median cmpTsv tsvAvg buffers.rightSpeeds in

    -- Naively assume that the speed is the average of the median speed of the
    -- two wheels.
    let speedRPM = divf (addf ls.1 rs.1) 2.0 in
    let speed = divf (mulf speedRPM wheelCircumference) 60.0 in

    -- We assume the timestamps of all observations are the same
    let ts = fld.0 in

    -- Translate sensor distances, which are given in centimeters, to meters.
    let fld = divf fld.1 100.0 in
    let frd = divf frd.1 100.0 in
    let rld = divf rld.1 100.0 in
    let rrd = divf rrd.1 100.0 in
    let sld = divf sld.1 100.0 in
    let srd = divf srd.1 100.0 in

    let posPosterior =
      infer (BPF {particles = 1000})
        (lam. positionModel roomMap state.posPriorTsv ts speed fld frd rld rrd sld srd)
    in
    match expectedValuePosDist posPosterior with (x, y) in
    printLn (join ["Expected value: x=", float2string x, ", y=", float2string y]);
    flushStdout ();

    let posteriorTsv = (ts, posPosterior) in

    writeData obsPosition posteriorTsv;

    {state with posPriorTsv = posteriorTsv, buffers = emptyBuffers ()}

  else {state with buffers = buffers}
)
