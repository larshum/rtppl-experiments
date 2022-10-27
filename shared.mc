include "buffers.mc"

let loopFn : all a. a -> (Int -> a -> a) -> a = lam v. lam f.
  recursive let work = lam i. lam v.
    let vnext = f i v in
    work (addi i 1) vnext
  in work 1 v

let printFloatBuffer : Int -> () = lam id.
  let buf = _loadBuffer id in
  let printTsv = lam tsv.
    match tsv with (ts, value) in
    join [int2string ts, " ", float2string (unsafeCoerce value)]
  in
  printLn (strJoin "\n" (map printTsv buf));
  exit 0

let printDist : all a. (a -> String) -> Dist a -> () =
  lam printSample. lam dist.
  recursive let work = lam samples. lam weights.
    match (samples, weights) with ([s] ++ samples, [w] ++ weights) then
      printLn (join [printSample s, " ", float2string w]);
      work samples weights
    else ()
  in
  match distEmpiricalSamples dist with (samples, weights) in
  work samples weights

let printFloatDistributionBuffer : Int -> () = lam id.
  let buf = _loadBuffer id in
  let printTsv = lam tsv.
    match tsv with (_, dist) in
    printDist float2string (unsafeCoerce dist)
  in
  iter printTsv buf;
  exit 0

let printPositionDistributionBuffer : Int -> () = lam id.
  let printPosSample : [Float] -> String = lam sample.
    -- NOTE: the angle is used when estimating, but it is not very interesting
    -- in the end.
    match sample with [x, y, _] in
    join [float2string x, " ", float2string y]
  in
  let buf = _loadBuffer id in
  let printTsv = lam tsv.
    match tsv with (_, dist) in
    printDist printPosSample (unsafeCoerce dist)
  in
  iter printTsv buf;
  exit 0

let handleOptions : Options -> () = lam options.
  if neqi options.printFloat (negi 1) then
    printFloatBuffer options.printFloat
  else if neqi options.printDist (negi 1) then
    printFloatDistributionBuffer options.printDist
  else if neqi options.printPosDist (negi 1) then
    printPositionDistributionBuffer options.printPosDist
  else ()

let cmpFloat : Float -> Float -> Int = lam l. lam r.
  if gtf l r then 1
  else if ltf l r then negi 1
  else 0

let floatAvg : Float -> Float -> Float = lam l. lam r.
  divf (addf l r) 2.0

let cmpTsv : (Int, Float) -> (Int, Float) -> Int = lam l. lam r.
  if gtf l.1 r.1 then 1
  else if ltf l.1 r.1 then negi 1
  else 0

let tsvAvg : (Int, Float) -> (Int, Float) -> (Int, Float) = lam l. lam r.
  match l with (tsl, vl) in
  match r with (tsr, vr) in
  (mini tsl tsr, floatAvg vl vr)

-- Finds the median among a given sequence of observations. If there is an even
-- number of observations, the median is given the minimum timestamp among the
-- two considered values.
let median : all a. (a -> a -> Int) -> (a -> a -> a) -> [a] -> a =
  lam cmp. lam merge. lam obs.
  let n = length obs in
  let obs = sort cmp obs in
  if eqi (modi n 2) 0 then
    let mid = divi n 2 in
    merge (get obs mid) (get obs (addi mid 1))
  else
    get obs (divi n 2)

let expectedValuePosDist : Dist [Float] -> [Float] = lam posPosterior.
  match distEmpiricalNormConst posPosterior with nc in
  match distEmpiricalSamples posPosterior with (samples, weights) in
  match
    foldl
      (lam acc : [Float]. lam t.
        match acc with [xAcc, yAcc, angleAcc] in
        match t with ([x, y, angle], w) in
        let nw = subf (exp w) nc in
        [addf xAcc (mulf nw x), addf yAcc (mulf nw y), addf angleAcc (mulf nw angle)])
      [0.0, 0.0, 0.0] (zip samples weights)
  with [x, y, angle] in
  let n = int2float (length samples) in
  [divf x n, divf y n, divf angle n]
