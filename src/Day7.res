open Belt

let data = Node_fs.readFileSync("./src/data/input_day7.txt", #utf8)

let sample = "16,1,2,0,4,2,7,1,2,14"

let positions = data->Js.String2.split(",")
  ->Array.map(nStr => nStr->Int.fromString->Option.getUnsafe)


// Part 1 *

let posSet = positions->Set.Int.fromArray

let min = posSet->Set.Int.minimum->Option.getUnsafe
let max = posSet->Set.Int.maximum->Option.getUnsafe

Js.log3("Position range: ", min, max)

let leastFuel = Array.range(min,max)
  ->Array.reduce(Js.Int.max, (leastFuel, pos) => {
    let candidate = positions->Array.reduce(0, (sum, n) =>
      sum + Js.Math.abs_int(pos - n))

    if candidate < leastFuel { candidate }
    else { leastFuel }
  })

Js.log2("Least fuel (constant rate): ", leastFuel)


// Part 2

let leastFuel = Array.range(min,max)
  ->Array.reduce(infinity, (leastFuel, pos) => {
    let candidate = positions->Array.reduce(0.0, (total, n) => {
      let delta = Js.Math.abs_int(pos - n)->Belt.Int.toFloat
      let deltaSum = (delta *. (delta +. 1.0)) /. 2.0

      total +. deltaSum
    })

    if candidate < leastFuel { candidate }
    else { leastFuel }
  })

Js.log2("Least fuel (linear rate): ", leastFuel)
