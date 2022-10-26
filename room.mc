include "math.mc"
include "string.mc"

-- We encode the room using a tensor of boolean values encoding each 10x10 cm
-- block. The value 'true' means that the corresponding block is obstructed,
-- e.g., because of a wall.
type RoomMap = Tensor[Bool]

-- Read the map encoding from a file. The first line contains the number of
-- rows and columns, separated by space. The second line contains the data,
-- which is a sequence of 0's and 1's of length equal to #rows * #cols.
let readMap : String -> RoomMap = lam filename.
  let fillTensor = lam t. lam rows.
    match tensorShape t with [nrows, ncols] in
    recursive
      let readRow = lam r.
        if eqi r nrows then ()
        else readCol r 0
      let readCol = lam r. lam c.
        if eqi c ncols then
          readRow (addi r 1)
        else
          let v = get (get rows r) c in
          (if eqc v '1' then tensorSetExn t [r, c] true else ());
          readCol r (addi c 1)
    in
    readRow 0;
    t
  in
  let s = readFile filename in
  match strSplit "\n" s with [coordsLine] ++ rows then
    match strSplit " " coordsLine with [nrows, ncols] then
      let nrows = string2int nrows in
      let ncols = string2int ncols in
      let t = tensorCreateDense [nrows, ncols] (lam. false) in
      fillTensor t rows
    else error "Invalid room map format"
  else error "Invalid room map format"

-- Pretty-printing of a map, used for debugging purposes
let printMap : RoomMap -> String = lam m.
  match tensorShape m with [nrows, ncols] in
  recursive
    let encodeRow : Int -> String = lam r.
      if eqi r nrows then []
      else join [encodeCol r 0, "\n", encodeRow (addi r 1)]
    let encodeCol : Int -> Int -> String = lam r. lam c.
      if eqi c ncols then ""
      else
        let b = tensorGetExn m [r, c] in
        let ch = if b then '1' else '0' in
        cons ch (encodeCol r (addi c 1))
  in
  let dataStr = encodeRow 0 in
  join [int2string nrows, " ", int2string ncols, "\n", dataStr]

-- Sanity check: reading and printing a test map should yield exactly the same
-- string as stored in the original file.
utest printMap (readMap "maps/test-map.txt") with readFile "maps/test-map.txt" using eqString

-- Converts a positional value (in either x- or y-axis) to a coordinate of the
-- map.
let positionToCoord : Float -> Int = lam x. floorfi (mulf x 10.0)

-- Determine whether the coordinate of the map corresponding to a given
-- position (x,y) is within bounds, i.e. whether it is not obstructed.
let withinRoomBounds : RoomMap -> Float -> Float -> Bool = lam m. lam x. lam y.
  not (tensorGetExn m [positionToCoord x, positionToCoord y])

-- Compute the expected distance needed to travel until we collide with a wall
-- or other obstructions in according to the map.
let expectedDistanceAngle : RoomMap -> Float -> Float -> Float -> Float =
  lam m. lam angle. lam x. lam y.
  let eps = 0.01 in
  recursive let work = lam accDist. lam x. lam y.
    if withinRoomBounds m x y then
      let x = addf x (mulf eps (cos angle)) in
      let y = addf y (mulf eps (sin angle)) in
      work (addf accDist eps) x y
    else accDist
  in work 0.0 x y

-- Use the function above, but shift the angle according to the direction in
-- which we should be looking in.
let expectedDistanceFront = expectedDistanceAngle

let expectedDistanceRear = lam m. lam angle.
  expectedDistanceAngle m (addf angle pi)

let expectedDistanceLeft = lam m. lam angle.
  expectedDistanceAngle m (addf angle (divf pi 2))

let expectedDistanceRight = lam m. lam angle.
  expectedDistanceAngle m (subf angle (divf pi 2))
