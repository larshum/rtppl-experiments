include "buffers.mc"

type FloatTsv = (Int, Float)

let loopFn : all a. a -> (Int -> a -> a) -> a = lam v. lam f.
  recursive let work = lam i. lam v.
    let vnext = f i v in
    work (addi i 1) vnext
  in work 1 v

let printFloatBuffer : String -> () = lam id.
  let buf = _loadBuffer id in
  let printTsv = lam tsv.
    match tsv with (ts, value) in
    join [int2string ts, " ", float2string (unsafeCoerce value)]
  in
  printLn (strJoin "\n" (map printTsv buf))

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

let printFloatDistributionBuffer : String -> () = lam id.
  let buf = _loadBuffer id in
  let printTsv = lam tsv.
    match tsv with (_, dist) in
    printDist float2string (unsafeCoerce dist)
  in
  iter printTsv buf

let printPositionDistributionBuffer : String -> () = lam id.
  let printPosSample : (Float, Float, Float) -> String = lam sample.
    match sample with (x, y, angle) in
    join [float2string x, " ", float2string y, " ", float2string angle]
  in
  let buf = _loadBuffer id in
  let printTsv = lam tsv.
    match tsv with (ts, dist) in
    printLn (int2string ts);
    printDist printPosSample (unsafeCoerce dist)
  in
  iter printTsv buf

let handleOptions : Options -> () = lam options.
  if null options.printBufferFiles then ()
  else (
    iter
      (lam entry.
        match entry with (id, bufType) in
        printError (join ["Printing contents of buffer file ", id, ":\n"]); flushStderr ();
        match bufType with FloatBuffer _ then
          printFloatBuffer id
        else match bufType with DistFloatBuffer _ then
          printFloatDistributionBuffer id
        else match bufType with DistPosBuffer _ then
          printPositionDistributionBuffer id
        else never)
      options.printBufferFiles;
    exit 0
  )

let cmpFloat : Float -> Float -> Int = lam l. lam r.
  if gtf l r then 1
  else if ltf l r then negi 1
  else 0

let floatAvg : Float -> Float -> Float = lam l. lam r.
  divf (addf l r) 2.0

let cmpTsv : FloatTsv -> FloatTsv -> Int = lam l. lam r.
  if gtf l.1 r.1 then 1
  else if ltf l.1 r.1 then negi 1
  else 0

let tsvAvg : FloatTsv -> FloatTsv -> FloatTsv = lam l. lam r.
  error "Cannot compute average timestamp"

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

let medianTsv : [FloatTsv] -> FloatTsv = median cmpTsv tsvAvg

let expectedValuePosDist : Dist (Float, Float, Float) -> (Float, Float, Float) = lam posPosterior.
  match distEmpiricalSamples posPosterior with (samples, weights) in
  foldl
    (lam acc. lam t.
      match acc with (xAcc, yAcc, angleAcc) in
      match t with ((x, y, angle), w) in
      let nw = exp w in
      (addf xAcc (mulf nw x), addf yAcc (mulf nw y), addf angleAcc (mulf nw angle)))
    (0.0, 0.0, 0.0) (zip samples weights)

let degToRad = lam angle.
  divf (mulf angle 180.0) pi

let radToDeg = lam angle.
  divf (mulf angle pi) 180.0
