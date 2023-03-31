include "ext/dist-ext.mc"
include "seq.mc"

include "argparse.mc"
include "room.mc"
include "shared.mc"

-- Collects the row and column indices of all blocks on the map that are not
-- obstructed.
let findNonObstructedBlocks : RoomMap -> [(Int, Int)] = lam m.
  match
    foldl
      (lam acc. lam row.
        match
          foldl
            (lam acc. lam obstructed.
              match acc with (accPos, (row, col)) in
              let accPos =
                if obstructed then accPos
                else cons (row, col) accPos
              in
              (accPos, (row, addi col 1)))
            acc row
        with (accPos, (row, _)) in
        (accPos, (addi row 1, 0)))
      ([], (0, 0)) m
  with (posIdxs, _) in
  posIdxs

-- Given a room map, chooses a random position on the map, among those that are
-- not obstructed.
let initialPositionModel : Int -> [(Int, Int)] -> State =
  lam t0. lam nonObstructedBlocks.
  match randElem nonObstructedBlocks with Some coord then
    match coordToPosition coord with (x, y) in
    let xOfs = assume (Uniform 0. 0.1) in
    let yOfs = assume (Uniform 0. 0.1) in
    let direction = assume (Uniform 0. (mulf 2. pi)) in
    { x = addf x xOfs, y = addf y yOfs, direction = direction, speed = 0.0,
      steeringAngle = 0.0, ts = t0 }
  else error "Provided room map is fully obstructed"

let initPosDist : Int -> RoomMap -> Dist State = lam t0. lam m.
  let blocks = findNonObstructedBlocks m in
  infer (Importance {particles = 1000}) (lam. initialPositionModel t0 blocks)
