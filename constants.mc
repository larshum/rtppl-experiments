
let wheelCircumference = 0.35

let maxLongRangeSensorDist = 4.0
let maxShortRangeSensorDist = 2.0

-- Encodes the offsets of the distance sensors on the car from its center. Each
-- sensor offset consists of an angle and a distance at which the sensor is
-- placed at, relative to the front direction of the car.
type SensorOffset = {angle : Float, dist : Float}
let frontLeftOfs = (0.209639845874, 0.240260275535)
let frontRightOfs = (negf 0.209639845874, 0.240260275535)
let rearLeftOfs = (2.76725903758, 0.30083217913)
let rearRightOfs = (negf 2.76725903758, 0.30083217913)
let leftOfs = (2.15879893034, 0.126194294641)
let rightOfs = (negf 2.15879893034, 0.126194294641)
