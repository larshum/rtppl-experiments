include "ext/dist-ext.mc"
include "../buffers.mc"

mexpr

let options = {optionsDefault with recording = true, recordBufferOnly = true} in

let inputs = [] in
let outputs = [distanceFrontLeft, distanceFrontRight, speedValLeft, speedValRight] in
initBuffers options inputs outputs;

-- Number of time steps
let n = 1000 in

-- Speed is (0.8 / 100) m/s, with some small variation
-- TODO: produce the number of rotations per seconds (wheel circumference is
-- ≈35cm)
let speedMsMu = divf 0.8 (int2float (divi n 10)) in
let speedRotMu = divf speedMsMu 0.35 in

-- Steady movement from 0.2m to 1.0m distance (front) over n time-steps.
loop n (lam i.
  let distMu = addf 0.2 (mulf 0.8 (divf (int2float i) (int2float n))) in

  let distLeft = gaussianSample distMu 0.02 in
  let distRight = gaussianSample distMu 0.02 in

  let speedLeft = gaussianSample speedRotMu 0.01 in
  let speedRight = gaussianSample speedRotMu 0.01 in

  -- Each timestamp is 100ms apart, to match the period in the 'runner'
  -- program.
  let ts = muli i 100 in
  writeData distanceFrontLeft (ts, distLeft);
  writeData distanceFrontRight (ts, distRight);
  writeData speedValLeft (ts, speedLeft);
  writeData speedValRight (ts, speedRight)
);

saveBuffersAndExit 2
