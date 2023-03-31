include "math.mc"
include "string.mc"

include "constants.mc"
include "shared.mc"

type Pos = (Float, Float)

-- We encode the room using a tensor of boolean values encoding each 10x10 cm
-- block. The value 'true' means that the corresponding block is obstructed,
-- e.g., because of a wall.
type RoomMap = [[Bool]]

let roomScaleFactor = 10.0

-- Read the map encoding from a file. The first line contains the number of
-- rows and columns, separated by space. The second line contains the data,
-- which is a sequence of 0's and 1's of length equal to #rows * #cols.
let readMap : String -> RoomMap = lam filename.
  let convChar = lam c. eqc c '1' in
  let s = strTrim (readFile filename) in
  match strSplit "\n" s with [coordsLine] ++ rows then
    match strSplit " " coordsLine with [nrows, ncols] then
      let nrows = string2int nrows in
      let ncols = string2int ncols in
      create nrows (lam r. map convChar (get rows r))
    else error "Invalid room map format"
  else error "Invalid room map format"

-- Pretty-printing of a map, used for debugging purposes
let printMap : RoomMap -> String = lam m.
  let printRow = lam row.
    snoc (map (lam b. if b then '1' else '0') row) '\n'
  in
  let nrows = length m in
  if eqi nrows 0 then "0 0"
  else
    let ncols = length (head m) in
    let dataStr = join (map (lam row. printRow row) m) in
    join [int2string nrows, " ", int2string ncols, "\n", dataStr]

-- Sanity check: reading and printing a test map should yield exactly the same
-- string as stored in the original file.
utest printMap (readMap "maps/track.txt") with readFile "maps/track.txt" using eqString

let roomDims : RoomMap -> (Int, Int) = lam m.
  let nrows = length m in
  if eqi nrows 0 then (0, 0)
  else (nrows, length (head m))

-- Converts a positional value (in either x- or y-axis) to a coordinate of the
-- map and back.
let positionToCoord : Pos -> (Int, Int) = lam xy.
  match xy with (x, y) in
  (floorfi (mulf y roomScaleFactor), floorfi (mulf x roomScaleFactor))

let eqCoord = lam l. lam r. and (eqi l.0 r.0) (eqi l.1 r.1)
utest positionToCoord (1.23, 0.5) with (5, 12) using eqCoord
utest positionToCoord (6.44, 3.99) with (39, 64) using eqCoord

let coordToPosition : (Int, Int) -> Pos = lam rowCol.
  match rowCol with (row, col) in
  (divf (int2float col) roomScaleFactor, divf (int2float row) roomScaleFactor)

let eqPos = lam l. lam r. and (eqf l.0 r.0) (eqf l.1 r.1)
utest coordToPosition (1, 0) with (0.0, 0.1) using eqPos
utest coordToPosition (positionToCoord (1.25, 0.037))
with  coordToPosition (positionToCoord (1.20, 0.0)) using eqPos

-- Determine whether the coordinate of the map corresponding to a given
-- position (x,y) is within bounds, i.e. whether it is not obstructed.
let withinRoomBounds : RoomMap -> Pos -> Bool = lam m. lam xy.
  match roomDims m with (nrows, ncols) in
  match positionToCoord xy with (row, col) in
  if or (or (lti row 0) (geqi row nrows)) (or (lti col 0) (geqi col ncols)) then
    false
  else not (get (get m row) col)

let positionPlusOffset : SensorOffset -> State -> State =
  lam offset. lam state.

  -- Compute the position at the provided offset.
  let direction = addf state.direction offset.angle in
  {state with x = addf state.x (mulf offset.dist (cos direction)),
              y = addf state.y (mulf offset.dist (sin direction))}

-- Compute the expected distance needed to travel until we collide with a wall
-- or other obstructions in according to the map.
let expectedDistanceState : RoomMap -> State -> Float =
  lam m. lam state.
  let eps = 0.05 in
  recursive let work = lam accDist. lam state.
    if withinRoomBounds m (state.x, state.y) then
      let state = {state with x = addf state.x (mulf eps (cos state.direction)),
                              y = addf state.y (mulf eps (sin state.direction))} in
      work (addf accDist eps) state
    else accDist
  in work 0.0 state

-- Use the function above, but shift the angle according to the direction in
-- which we should be looking in.
let expectedDistanceFront = lam m. lam ofs. lam state.
  let state = positionPlusOffset ofs state in
  expectedDistanceState m state

let expectedDistanceRear = lam m. lam ofs. lam state.
  let state = positionPlusOffset ofs state in
  expectedDistanceState m {state with direction = addf state.direction pi}

let expectedDistanceLeft = lam m. lam ofs. lam state.
  let state = positionPlusOffset ofs state in
  expectedDistanceState m {state with direction = subf state.direction (divf pi 2.0)}

let expectedDistanceRight = lam m. lam ofs. lam state.
  let state = positionPlusOffset ofs state in
  expectedDistanceState m {state with direction = addf state.direction (divf pi 2.0)}

mexpr

let statePos = lam s.
  (s.x, s.y)
in
let testPos = lam ofs. lam s.
  statePos (positionPlusOffset ofs s)
in
let eqTupleApprox = lam l. lam r.
  if eqfApprox 1e-10 l.0 r.0 then
    eqfApprox 1e-10 l.1 r.1
  else false
in

let s = {x = 0.0, y = 0.0, direction = 0.0, speed = 0.0, steeringAngle = 0.0, ts = 0} in
utest testPos frontLeftOfs s with (0.235, 0.05) using eqTupleApprox in
utest testPos frontRightOfs s with (0.235, negf 0.05) using eqTupleApprox in
utest testPos rearLeftOfs s with (negf 0.28, 0.11) using eqTupleApprox in
utest testPos rearRightOfs s with (negf 0.28, negf 0.11) using eqTupleApprox in
utest testPos leftOfs s with (negf 0.07, 0.105) using eqTupleApprox in
utest testPos rightOfs s with (negf 0.07, negf 0.105) using eqTupleApprox in

let s2 = {s with direction = divf pi 2.0} in
utest testPos frontLeftOfs s2 with (negf 0.05, 0.235) using eqTupleApprox in
utest testPos frontRightOfs s2 with (0.05, 0.235) using eqTupleApprox in
utest testPos rearLeftOfs s2 with (negf 0.11, negf 0.28) using eqTupleApprox in
utest testPos rearRightOfs s2 with (0.11, negf 0.28) using eqTupleApprox in
utest testPos leftOfs s2 with (negf 0.105, negf 0.07) using eqTupleApprox in
utest testPos rightOfs s2 with (0.105, negf 0.07) using eqTupleApprox in

()
