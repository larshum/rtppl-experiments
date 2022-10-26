include "ext/dist-ext.mc"
include "../buffers.mc"

mexpr

let options = {optionsDefault with recording = true, recordBufferOnly = true} in

let inputs = [] in
let outputs = [
  distanceFrontLeft, distanceFrontRight, distanceBackLeft, distanceBackRight,
  distanceSideLeft, distanceSideRight, speedValLeft, speedValRight, startTime
] in
initBuffers options inputs outputs;

-- Number of time steps
let n = 100 in

-- Speed is -0.08 m/s, with some small variation. We produce it as the number
-- of rotations per minute (RPM), as that is the input format (wheel circumference
-- is ≈35cm). This encodes constant speed movement resulting in the front
-- distance increasing from 0.2m to 1.0m.
let speedMsMu = negf 0.08 in
let speedRotMu = divf (mulf 60.0 speedMsMu) 0.35 in
let frontDistMuRef = ref 0.2 in
let rearDistMuRef = ref 1.8 in

writeData startTime (0, 0);

loop n (lam i.
  -- Each timestamp is 100ms (100*10^6 ns) apart, to match the period in the
  -- 'runner' program.
  let ts = muli i 100000000 in

  -- Compute the new distance (front and rear) as the previous distance plus
  -- the constant speed times the time since the last time-stamp, which is
  -- always 100ms (=0.1s).
  let frontDistMu = subf (deref frontDistMuRef) (mulf speedMsMu 0.1) in
  let rearDistMu = addf (deref rearDistMuRef) (mulf speedMsMu 0.1) in
  modref frontDistMuRef frontDistMu;
  modref rearDistMuRef rearDistMu;

  let distFrontLeft = gaussianSample frontDistMu 0.02 in
  let distFrontRight = gaussianSample frontDistMu 0.02 in
  let distRearLeft = gaussianSample rearDistMu 0.02 in
  let distRearRight = gaussianSample rearDistMu 0.02 in

  let speedLeft = gaussianSample speedRotMu 0.01 in
  let speedRight = gaussianSample speedRotMu 0.01 in

  writeData distanceFrontLeft (ts, distFrontLeft);
  writeData distanceFrontRight (ts, distFrontRight);
  writeData distanceBackLeft (ts, distRearLeft);
  writeData distanceBackRight (ts, distRearRight);
  writeData speedValLeft (ts, speedLeft);
  writeData speedValRight (ts, speedRight)
  --printLn (join [int2string ts, " ", float2string distMu])
);

saveBuffersAndExit 2
