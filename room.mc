include "math.mc"
include "string.mc"

-- We encode the room using a tensor of boolean values encoding each 10x10 cm
-- block. The value 'true' means that the corresponding block is obstructed,
-- e.g., because of a wall.
type RoomMap = [[Bool]]

-- Read the map encoding from a file. The first line contains the number of
-- rows and columns, separated by space. The second line contains the data,
-- which is a sequence of 0's and 1's of length equal to #rows * #cols.
let readMap : String -> RoomMap = lam filename.
  let convChar = lam c. eqc c '1' in
  let s = readFile filename in
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
utest printMap (readMap "maps/test-map.txt") with readFile "maps/test-map.txt" using eqString

let roomDims : RoomMap -> (Int, Int) = lam m.
  let nrows = length m in
  if eqi nrows 0 then (0, 0)
  else (nrows, length (head m))

-- Converts a positional value (in either x- or y-axis) to a coordinate of the
-- map and back.
let positionToCoord : (Float, Float) -> (Int, Int) = lam xy.
  match xy with (x, y) in
  (floorfi (mulf y 10.0), floorfi (mulf x 10.0))

let coordToPosition : (Int, Int) -> (Float, Float) = lam rowCol.
  match rowCol with (row, col) in
  (divf (int2float col) 10.0, divf (int2float row) 10.0)

-- Determine whether the coordinate of the map corresponding to a given
-- position (x,y) is within bounds, i.e. whether it is not obstructed.
let withinRoomBounds : RoomMap -> (Float, Float) -> Bool = lam m. lam xy.
  match positionToCoord xy with (row, col) in
  not (get (get m row) col)

-- Compute the expected distance needed to travel until we collide with a wall
-- or other obstructions in according to the map.
let expectedDistanceAngle : RoomMap -> Float -> (Float, Float) -> Float =
  lam m. lam angle. lam xy.
  let eps = 0.05 in
  recursive let work = lam accDist. lam xy.
    if withinRoomBounds m xy then
      match xy with (x, y) in
      let x = addf x (mulf eps (cos angle)) in
      let y = addf y (mulf eps (sin angle)) in
      work (addf accDist eps) (x, y)
    else accDist
  in work 0.0 xy

-- Use the function above, but shift the angle according to the direction in
-- which we should be looking in.
let expectedDistanceFront = expectedDistanceAngle

let expectedDistanceRear = lam m. lam angle.
  expectedDistanceAngle m (addf angle pi)

let expectedDistanceLeft = lam m. lam angle.
  expectedDistanceAngle m (addf angle (divf pi 2.0))

let expectedDistanceRight = lam m. lam angle.
  expectedDistanceAngle m (subf angle (divf pi 2.0))
