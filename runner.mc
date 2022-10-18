include "ext/rtppl-ext.mc"
include "common.mc"
include "option.mc"
include "string.mc"

include "argparse.mc"
include "buffers.mc"
include "distance-model.mc"
include "sm_conf.mc"

let loopFn : all a. a -> (Int -> a -> a) -> a = lam v. lam f.
  recursive let work = lam i. lam v.
    let vnext = f i v in
    work (addi i 1) vnext
  in
  work 0 v

mexpr

let options = parseOptions (tail argv) in

-- User has to declare the input and output sensors explicitly.
let inputs = [distanceFrontLeft, distanceFrontRight] in
let outputs = [obsDistanceFront] in
initBuffers options inputs outputs;

let cmpTsv : (Int, Float) -> (Int, Float) -> Int = lam l. lam r.
  if gtf l.1 r.1 then 1
  else if ltf l.1 r.1 then negi 1
  else 0
in

-- Finds the median among a given sequence of observations. If there is an even
-- number of observations, the median is given the minimum timestamp among the
-- two considered values.
let median : [(Int, Float)] -> (Int, Float) = lam obs.
  let n = length obs in
  let obs = sort cmpTsv obs in
  if eqi (modi n 2) 0 then
    let fst = divi n 2 in
    match get obs fst with (ts1, v1) in
    match get obs (addi fst 1) with (ts2, v2) in
    (mini ts1 ts2, divf (addf v1 v2) 2.0)
  else
    get obs (divi n 2)
in

let leftDists = ref [] in
let rightDists = ref [] in

loopFn (Uniform 0.0 4.0) (lam i. lam prior.
  sleepMs 100;

  let frontLeft = readFloatData distanceFrontLeft in
  let frontRight = readFloatData distanceFrontRight in

  modref leftDists (snoc (tail (deref leftDists)) frontLeft);
  modref rightDists (snoc (tail (deref rightDists)) frontRight);

  -- Only run the model once per second
  if eqi (modi i 10) 0 then
    let lobs = median (deref leftDists) in
    modref leftDists [];
    let robs = median (deref rightDists) in
    modref rightDists [];

    let posterior = infer (Importance {particles = 1000}) (lam. distanceModel prior lobs robs) in

    match (lobs, robs) with ((ts1, _), (ts2, _)) in
    let ts = mini ts1 ts2 in
    writeData obsDistanceFront (ts, posterior);

    posterior
  else
    prior
)
