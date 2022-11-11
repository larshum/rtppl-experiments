include "math.mc"

include "argparse.mc"
include "constants.mc"
include "shared.mc"
include "room.mc"

let positionModel : RoomMap -> (Int, Dist [Float]) -> Int -> Float -> Float
                 -> Float -> Float -> Float -> Float -> Float -> [Float] =
  lam m. lam posPriorTsv. lam t1. lam speed. lam frontLeftObs. lam frontRightObs.
  lam rearLeftObs. lam rearRightObs. lam leftDistObs. lam rightDistObs.

  -- Compute the maximum possible distance (the diagonal of the room)
  match coordToPosition (roomDims m) with (maxX, maxY) in
  let maxDist = sqrt (addf (mulf maxX maxX) (mulf maxY maxY)) in

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
    let pos =
      let distTravelled = assume (Gaussian (mulf speed timeDiff) 0.2) in
      let x1 = addf x0 (mulf distTravelled (cos newAngle)) in
      let y1 = addf y0 (mulf distTravelled (sin newAngle)) in
      (x1, y1)
    in

    -- Check whether the position we presumably moved to is also within bounds.
    -- Otherwise we could not have moved there.
    (if withinRoomBounds m pos then
      -- If an observed distance is beyond the maximum range of the sensor, we
      -- only know that the actual distance is anything between that maximum
      -- range and the maximum distance we can observe in the room.
      let obs = lam maxRange. lam obsDist.
        if ltf obsDist maxRange then obsDist
        else assume (Uniform maxRange maxDist)
      in

      -- Compute the likelihood of making the observations given the assumed
      -- position (including angle) of the car. As the goal of the model is to
      -- estimate the position (of the center) of the car, we take the offsets of
      -- the individual sensors into account in the model.

      let frontLeftObs = obs maxLongRangeSensorDist frontLeftObs in
      let frontLeftDist = expectedDistanceFront m newAngle pos frontLeftOfs in
      observe frontLeftObs (Gaussian frontLeftDist 0.1);

      let frontRightObs = obs maxLongRangeSensorDist frontRightObs in
      let frontRightDist = expectedDistanceFront m newAngle pos frontRightOfs in
      observe frontRightObs (Gaussian frontRightDist 0.1);

      let rearLeftObs = obs maxLongRangeSensorDist rearLeftObs in
      let rearLeftDist = expectedDistanceRear m newAngle pos rearLeftOfs in
      observe rearLeftObs (Gaussian rearLeftDist 0.1);

      let rearRightObs = obs maxLongRangeSensorDist rearRightObs in
      let rearRightDist = expectedDistanceRear m newAngle pos rearRightOfs in
      observe rearRightObs (Gaussian rearRightDist 0.1);

      let leftDistObs = obs maxShortRangeSensorDist leftDistObs in
      let leftDist = expectedDistanceLeft m newAngle pos leftOfs in
      observe leftDistObs (Gaussian leftDist 0.05);

      let rightDistObs = obs maxShortRangeSensorDist rightDistObs in
      let rightDist = expectedDistanceRight m newAngle pos rightOfs in
      observe rightDistObs (Gaussian rightDist 0.05)
    else
      weight (negf inf));

    [pos.0, pos.1, newAngle]
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

    -- Ignore the individual timestamps in the model code
    let fld = fld.1 in
    let frd = frd.1 in
    let rld = rld.1 in
    let rrd = rrd.1 in
    let sld = sld.1 in
    let srd = srd.1 in

    let posPosterior =
      infer (BPF {particles = 1000})
        (lam. positionModel roomMap state.posPriorTsv ts speed fld frd rld rrd sld srd)
    in
    match expectedValuePosDist posPosterior with [x, y, _] in
    printLn (join ["Expected value: x=", float2string x, ", y=", float2string y]);
    flushStdout ();

    let posteriorTsv = (ts, posPosterior) in

    writeData obsPosition posteriorTsv;

    {state with posPriorTsv = posteriorTsv, buffers = emptyBuffers ()}

  else {state with buffers = buffers}
)
