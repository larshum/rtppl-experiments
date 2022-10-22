include "ext/dist-ext.mc"
include "../buffers.mc"

mexpr

let options = {optionsDefault with recording = true, recordBufferOnly = true} in

let inputs = [] in
let outputs = [distanceFrontLeft, distanceFrontRight, speedValLeft, speedValRight] in
initBuffers options inputs outputs;

-- Number of time steps
let n = 1000 in

-- Speed is -(0.8 / 100) m/s, with some small variation
-- TODO: produce the number of rotations per seconds (wheel circumference is
-- ≈35cm)
let speedMsMu = negf (divf 0.8 (int2float (divi n 10))) in
let speedRotMu = divf speedMsMu 0.35 in
let distMuRef = ref 0.2 in

-- Steady movement from 0.2m to 1.0m distance (front) over n time-steps.
loop n (lam i.
  -- Each timestamp is 100ms apart, to match the period in the 'runner'
  -- program.
  let ts = muli i 100 in

  -- Compute the new distance as the previous distance plus the constant speed
  -- times the time since the last time-stamp, which is always 100ms (=0.1s).
  let distMu = subf (deref distMuRef) (mulf speedMsMu 0.1) in
  modref distMuRef distMu;

  let distLeft = gaussianSample distMu 0.02 in
  let distRight = gaussianSample distMu 0.02 in

  let speedLeft = gaussianSample speedRotMu 0.001 in
  let speedRight = gaussianSample speedRotMu 0.001 in

  writeData distanceFrontLeft (ts, distLeft);
  writeData distanceFrontRight (ts, distRight);
  writeData speedValLeft (ts, speedLeft);
  writeData speedValRight (ts, speedRight);
  printLn (join [int2string ts, " ", float2string distMu])
);

saveBuffersAndExit 2
