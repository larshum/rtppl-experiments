include "buffers.mc"

type FloatTsv = (Int, Float)

type State = {
  x : Float,
  y : Float,
  speed : Float,
  direction : Float,
  steeringAngle : Float,
  ts : Int
}

let tan = lam rad. divf (sin rad) (cos rad)

let loopFn : all a. a -> (Int -> a -> a) -> a = lam v. lam f.
  recursive let work = lam i. lam v.
    let vnext = f i v in
    work (addi i 1) vnext
  in work 1 v

let cmpFloat : Float -> Float -> Int = lam l. lam r.
  if gtf l r then 1
  else if ltf l r then negi 1
  else 0

let floatAvg : Float -> Float -> Float = lam l. lam r.
  divf (addf l r) 2.0

let cmpTsvTime : all a. (Int, a) -> (Int, a) -> Int = lam l. lam r.
  if gti l.0 r.0 then 1
  else if lti l.0 r.0 then negi 1
  else 0

let cmpTsv : FloatTsv -> FloatTsv -> Int = lam l. lam r.
  if gtf l.1 r.1 then 1
  else if ltf l.1 r.1 then negi 1
  else 0

let tsvAvg : FloatTsv -> FloatTsv -> FloatTsv = lam l. lam r.
  error "Cannot compute average timestamp"

-- Finds the median among a given sequence of observations. If there is an even
-- number of observations, the median is given the minimum timestamp among the
-- two considered values.
let medianTsv : [FloatTsv] -> FloatTsv = lam obs.
  let n = length obs in
  let obs = sort cmpTsv obs in
  if eqi (modi n 2) 0 then
    let mid = divi n 2 in
    tsvAvg (get obs mid) (get obs (addi mid 1))
  else
    get obs (divi n 2)

let degToRad = lam angle.
  divf (mulf angle pi) 180.0

let radToDeg = lam angle.
  divf (mulf angle 180.0) pi
