include "ext/dist-ext.mc"
include "buffers.mc"
include "constants.mc"
include "room.mc"

let backwardMovementConstantSpeed = lam room.
  -- Number of time steps
  let n = 200 in

  -- Speed is constant at -0.06 m/s. We output it as RPM, as that is the input
  -- from sensors.
  let speedMs = negf 0.06 in
  let speedRPM = divf (mulf 60.0 speedMs) wheelCircumference in

  -- Car moves backwards along the x-axis at a constant speed
  let initPos = (1.6, 1.0) in
  let pos = ref initPos in

  writeData startTime (0, 0);

  loop n (lam i.
    -- Each timestamp is 100ms (100*10^6 ns) apart, to match the period in the
    -- 'runner' program.
    let ts = muli i 100000000 in

    -- Compute the new actual position based on the movement. We only move along
    -- the x-axis, for simplicity.
    match deref pos with (x0, y0) in
    let newPos = (addf x0 (mulf speedMs 0.1), y0) in
    modref pos newPos;

    -- Compute the actual distances (rough estimates) in all four directions in
    -- centimeters, based on the provided map.
    let frontLeftDist = mulf (expectedDistanceFront room 0.0 newPos frontLeftOfs) 100.0 in
    let frontRightDist = mulf (expectedDistanceFront room 0.0 newPos frontRightOfs) 100.0 in
    let rearLeftDist = mulf (expectedDistanceRear room 0.0 newPos rearLeftOfs) 100.0 in
    let rearRightDist = mulf (expectedDistanceRear room 0.0 newPos rearRightOfs) 100.0 in
    let leftDist = mulf (expectedDistanceLeft room 0.0 newPos leftOfs) 100.0 in
    let rightDist = mulf (expectedDistanceRight room 0.0 newPos rightOfs) 100.0 in

    -- Produce estimates to simulate the noise of the actual sensors.
    let distFrontLeft = gaussianSample frontLeftDist 0.02 in
    let distFrontRight = gaussianSample frontRightDist 0.02 in
    let distRearLeft = gaussianSample rearLeftDist 0.02 in
    let distRearRight = gaussianSample rearRightDist 0.02 in
    let distSideLeft = gaussianSample leftDist 0.02 in
    let distSideRight = gaussianSample rightDist 0.02 in

    let speedLeft = gaussianSample speedRPM 0.01 in
    let speedRight = gaussianSample speedRPM 0.01 in

    writeData distanceFrontLeft (ts, distFrontLeft);
    writeData distanceFrontRight (ts, distFrontRight);
    writeData distanceBackLeft (ts, distRearLeft);
    writeData distanceBackRight (ts, distRearRight);
    writeData distanceSideLeft (ts, distSideLeft);
    writeData distanceSideRight (ts, distSideRight);
    writeData speedValLeft (ts, speedLeft);
    writeData speedValRight (ts, speedRight))

mexpr

let options = {optionsDefault with recording = true, recordBufferOnly = true} in

let room =
  let mapFilename =
    if gti (length argv) 1 then get argv 1
    else "maps/simple-map.txt"
  in
  readMap mapFilename
in

let inputs = [] in
let outputs = [
  distanceFrontLeft, distanceFrontRight, distanceBackLeft, distanceBackRight,
  distanceSideLeft, distanceSideRight, speedValLeft, speedValRight, startTime
] in
initBuffers options inputs outputs;

backwardMovementConstantSpeed room;

saveBuffersAndExit 2
