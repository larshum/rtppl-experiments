include "math.mc"
include "ext/dist-ext.mc"

include "argparse.mc"
include "constants.mc"
include "shared.mc"
include "room.mc"

type FloatTsv = (Int, Float)

let estimatePositionAt : (Float, Float) -> Float -> Float -> Int -> Int -> (Float, Float) =
  lam initialPos. lam speed. lam angle. lam t0. lam t1.
  match initialPos with (x0, y0) in

  -- Compute difference between timestamps and convert difference to a time
  -- in seconds.
  let timeDiff =
    if eqi t0 0 then 0.0
    else divf (int2float (subi t1 t0)) 1000000000.0
  in

  -- Estimate the distance travelled based on the estimated speed and the time
  -- difference. Given the angle in which the car is travelling, we use this to
  -- estimate the new position of the car.
  let distEst = mulf speed timeDiff in
  let distForward = assume (Gaussian distEst (mulf distEst 0.1)) in
  ( addf x0 (mulf distForward (cos angle))
  , addf y0 (mulf distForward (sin angle)) )

let positionModel : RoomMap -> (Int, Dist [Float]) -> Int -> Float -> FloatTsv
                 -> FloatTsv -> FloatTsv -> FloatTsv -> FloatTsv -> FloatTsv
                 -> [Float] =
  lam m. lam posPriorTsv. lam t1. lam speed. lam frontLeft. lam frontRight.
  lam rearLeft. lam rearRight. lam leftSide. lam rightSide.

  -- Compute the maximum possible distance (the diagonal of the room)
  match coordToPosition (roomDims m) with (maxX, maxY) in
  let maxDist = sqrt (addf (mulf maxX maxX) (mulf maxY maxY)) in

  -- Get an estimate of the previous position of the car.
  match posPriorTsv with (t0, posPrior) in
  match assume posPrior with [x0, y0, angle] in
  let initPos = (x0, y0) in

  if withinRoomBounds m initPos then

    -- There is some degree of uncertainty in what the actual speed is.
    let speed = assume (Gaussian speed 0.025) in

    -- TODO: How do we accurately estimate the angle? For now, we assume it is
    -- a fixed value.
    --let newAngle = assume (Gaussian angle (divf pi 4.0)) in
    let newAngle = pi in

    -- Estimate the current position given speed and time difference, with some
    -- uncertainty.
    let pos = estimatePositionAt initPos speed newAngle t0 t1 in

    -- Check whether the position we presumably moved to is also within bounds.
    -- If it is not, we could not have moved there, so we weight with negative
    -- infinity.
    (if withinRoomBounds m pos then
      -- If an observed distance is beyond the maximum range of the sensor, we
      -- only know that the actual distance is anything between that maximum
      -- range and the maximum distance we can observe in the room.
      let obs = lam maxRange. lam obsDist.
        if ltf obsDist maxRange then obsDist
        else assume (Uniform maxRange maxDist)
      in

      -- Compute the likelihood of making the provided observations at the
      -- estimated position of the car at their respective timestamps. The
      -- offsets of the sensors are taken into account when making these
      -- observations, as we seek to estimate the position relative to a
      -- central point of the car.
      -- TODO: The distance sensor values need to have timestamps relative to
      -- the previous release (t0), so that we can compute what they _should_
      -- have been.
      match frontLeft with (tsFl, frontLeftObs) in
      --let pos = estimatePositionAt initPos speed newAngle t0 tsFl in
      let frontLeftObs = obs maxLongRangeSensorDist frontLeftObs in
      let frontLeftDist = expectedDistanceFront m newAngle pos frontLeftOfs in
      observe frontLeftObs (Gaussian frontLeftDist 0.1);

      match frontRight with (tsFr, frontRightObs) in
      --let pos = estimatePositionAt initPos speed newAngle t0 tsFr in
      let frontRightObs = obs maxLongRangeSensorDist frontRightObs in
      let frontRightDist = expectedDistanceFront m newAngle pos frontRightOfs in
      observe frontRightObs (Gaussian frontRightDist 0.1);

      match rearLeft with (tsRl, rearLeftObs) in
      --let pos = estimatePositionAt initPos speed newAngle t0 tsRl in
      let rearLeftObs = obs maxLongRangeSensorDist rearLeftObs in
      let rearLeftDist = expectedDistanceRear m newAngle pos rearLeftOfs in
      observe rearLeftObs (Gaussian rearLeftDist 0.1);

      match rearRight with (tsRr, rearRightObs) in
      --let pos = estimatePositionAt initPos speed newAngle t0 tsRr in
      let rearRightObs = obs maxLongRangeSensorDist rearRightObs in
      let rearRightDist = expectedDistanceRear m newAngle pos rearRightOfs in
      observe rearRightObs (Gaussian rearRightDist 0.1);

      match leftSide with (tsLeft, leftSideObs) in
      --let pos = estimatePositionAt initPos speed newAngle t0 tsLeft in
      let leftSideObs = obs maxShortRangeSensorDist leftSideObs in
      let leftSide = expectedDistanceLeft m newAngle pos leftOfs in
      observe leftSideObs (Gaussian leftSide 0.05);

      match rightSide with (tsRight, rightSideObs) in
      --let pos = estimatePositionAt initPos speed newAngle t0 tsRight in
      let rightSideObs = obs maxShortRangeSensorDist rightSideObs in
      let rightSide = expectedDistanceRight m newAngle pos rightOfs in
      observe rightSideObs (Gaussian rightSide 0.05);

      printLn (join ["prior pos: ", float2string x0, " ", float2string y0]);
      printLn (join ["| ", float2string frontLeftObs, " ", float2string frontLeftDist, " ", float2string (gaussianLogPdf frontLeftDist 0.1 frontLeftObs)]);
      printLn (join ["| ", float2string frontRightObs, " ", float2string frontRightDist, " ", float2string (gaussianLogPdf frontRightDist 0.1 frontRightObs)]);
      printLn (join ["| ", float2string rearLeftObs, " ", float2string rearLeftDist, " ", float2string (gaussianLogPdf rearLeftDist 0.1 rearLeftObs)]);
      printLn (join ["| ", float2string rearRightObs, " ", float2string rearRightDist, " ", float2string (gaussianLogPdf rearRightDist 0.1 rearRightObs)]);
      printLn (join ["| ", float2string leftSideObs, " ", float2string leftSide, " ", float2string (gaussianLogPdf leftSide 0.05 leftSideObs)]);
      printLn (join ["| ", float2string rightSideObs, " ", float2string rightSide, " ", float2string (gaussianLogPdf rightSide 0.05 rightSideObs)])
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
printLn (join ["Room dimensions: ", float2string maxX, " ", float2string maxY]);

let positionPrior =
  distCombineIndependent
    [ Uniform 0.0 maxX -- prior for x-coordinate
    , Uniform 0.0 maxY -- prior for y-coordinate
    , Uniform 0.0 (mulf 2.0 pi) ] -- prior for the angle (direction of the car)
in
let state = {
  posPriorTsv = (0, positionPrior),
  buffers = emptyBuffers ()
} in

let n = 9 in
let period = 100 in

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

    -- NOTE(larshum, 2022-11-14): We assume that the observed speed is constant
    -- during the whole period. It is computed as the average of the median
    -- speed of the observations of the left and right wheels.
    let speedRPM = divf (addf ls.1 rs.1) 2.0 in
    let speedObs = divf (mulf speedRPM wheelCircumference) 60.0 in

    -- Compute the next timestamp based on the timestamp of the prior
    -- estimation and the period.
    let t1 = addi prevTs (muli n (muli period 1000000)) in

    let posPosterior =
      infer (BPF {particles = 1000})
        (lam. positionModel roomMap state.posPriorTsv t1 speedObs fld frd rld rrd sld srd)
    in
    match expectedValuePosDist posPosterior with [x, y, _] in
    printLn (join ["Expected value: x=", float2string x, ", y=", float2string y]);
    flushStdout ();

    let posteriorTsv = (t1, posPosterior) in

    writeData obsPosition posteriorTsv;

    {state with posPriorTsv = posteriorTsv, buffers = emptyBuffers ()}

  else {state with buffers = buffers}
)
