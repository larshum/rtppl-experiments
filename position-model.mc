include "math.mc"

include "argparse.mc"
include "shared.mc"
include "room.mc"

let wheelCircumference = 0.35

let positionModel : RoomMap -> (Int, Dist [Float]) -> Int
                 -> Float -> Float -> Float -> Float -> Float -> [Float] =
  lam m. lam posPriorTsv. lam t1. lam speed. lam frontDistObs. lam rearDistObs.
  lam leftDistObs. lam rightDistObs.

  -- Get an estimate of the previous position of the car.
  match posPriorTsv with (t0, posPrior) in
  match assume posPrior with [x0, y0, angle] in

  -- NOTE: we assume all observations have the same timestamp
  if withinRoomBounds m (x0, y0) then

    -- NOTE: this could probably be improved by considering the distance the
    -- wheels have travelled.
    let newAngle = assume (Gaussian angle pi) in

    -- Compute difference between timestamps and convert difference to a time
    -- in seconds.
    let timeDiff = divf (int2float (subi t1 t0)) 1000000000.0 in

    let dist = assume (Gaussian (mulf speed timeDiff) 0.02) in
    let x1 = addf x0 (mulf dist (cos newAngle)) in
    let y1 = addf y0 (mulf dist (sin newAngle)) in

    let frontDist = expectedDistanceFront m newAngle (x1, y1) in
    observe frontDistObs (Gaussian frontDist 0.02);

    let rearDist = expectedDistanceRear m newAngle (x1, y1) in
    observe rearDistObs (Gaussian rearDist 0.02);

    let leftDist = expectedDistanceLeft m newAngle (x1, y1) in
    observe leftDistObs (Gaussian leftDist 0.02);

    let rightDist = expectedDistanceRight m newAngle (x1, y1) in
    observe rightDistObs (Gaussian rightDist 0.02);

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
  -- TODO: Replace sleepMs with sdelay
  (if options.replaying then ()
  else sleepMs 100);

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

    -- NOTE: for now, we only consider the observations of the left front and
    -- rear sensors, to keep things simple(r).
    let posPosterior =
      infer (BPF {particles = 1000})
        (lam. positionModel roomMap state.posPriorTsv ts speed fld.1 rld.1 sld.1 srd.1)
    in
    match distEmpiricalSamples posPosterior with (samples, _) in
    match samples with [xValues, yValues] ++ _ in
    printLn (join [
      "Mean values: x = ", float2string (median cmpFloat floatAvg xValues),
      ", y = ", float2string (median cmpFloat floatAvg yValues)]);

    let posteriorTsv = (ts, posPosterior) in

    writeData obsPosition posteriorTsv;

    {state with posPriorTsv = posteriorTsv, buffers = emptyBuffers ()}

  else {state with buffers = buffers}
)
