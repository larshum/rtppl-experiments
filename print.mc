

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
  let printPosSample : State -> String = lam s.
    join [float2string s.x, " ", float2string s.y, " ", float2string s.direction]
  in
  let buf = _loadBuffer id in
  let printTsv = lam tsv.
    match tsv with (ts, dist) in
    printLn (int2string ts);
    printDist printPosSample (unsafeCoerce dist)
  in
  iter printTsv buf

let expectedValuePosDist : Dist State -> State = lam posPosterior.
  match distEmpiricalSamples posPosterior with (samples, weights) in
  foldl
    (lam acc. lam t.
      match t with (state, w) in
      let nw = exp w in
      {acc with x = addf acc.x (mulf nw state.x),
                y = addf acc.y (mulf nw state.y),
                speed = addf acc.speed (mulf nw state.speed),
                direction = addf acc.direction (mulf nw state.direction),
                steeringAngle = 0.0, ts = 0})
    {x = 0.0, y = 0.0, speed = 0.0, direction = 0.0, steeringAngle = 0.0, ts = 0}
    (zip samples weights)

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
