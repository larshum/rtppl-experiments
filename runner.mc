include "ext/rtppl-ext.mc"
include "common.mc"
include "option.mc"
include "string.mc"

include "argparse.mc"
include "buffers.mc"
-- include "distance-model.mc"
include "sm_conf.mc"

recursive let loopFn : all a. a -> (a -> a) -> a = lam v. lam f.
  let vnext = f v in
  loopFn vnext f
end

mexpr

-- TODO: these outputs should be declared in the 'sm_conf.mc' file
let obsDistanceFront = 8 in

let options = parseOptions (tail argv) in

-- User has to declare these
let inputs = [distanceFrontLeft, distanceFrontRight] in
let outputs = [obsDistanceFront] in

let state = init options inputs outputs in

loopFn 0.0 (lam prior.
  sleepMs 20;

  match readData () with [tsv1, tsv2] in

  -- TODO: insert PPL model code here
  let posterior = prior in

  -- TODO: support different types, so that we can write the posterior data
  -- here...
  writeData [(obsDistanceFront, (0, 0.0))];

  posterior
)
