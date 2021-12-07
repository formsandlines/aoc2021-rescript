let raw = Node_fs.readFileSync("./src/data/input_day1.txt", #utf8)

let sample = "199
200
208
210
200
207
240
269
260
263"

let data = raw->Js.String2.split("\n")
  ->Belt.Array.keep(str => str != "")
  ->Belt.Array.mapU((. str) => str->Belt.Int.fromString->Belt.Option.getExn)


// Part 1 *

let increases = data->Belt.Array.keepWithIndexU((. n,i) => {
  switch data->Belt.Array.get(i-1) {
    | Some(nPrev) => n > nPrev
    | None => false
  }
})

let incrCount = increases->Belt.Array.length

Js.log3("Part 1: ", increases, incrCount)


// Part 2 *

let sum = ref(999999)
let increases = data->Belt.Array.keepWithIndexU((. n1,i) => {
  switch (data->Belt.Array.get(i+1),
          data->Belt.Array.get(i+2)) {
    | (Some(n2),Some(n3)) => {
        let prevSum = sum.contents
        sum := (n1 + n2 + n3)
        sum.contents > prevSum
      }
    | _ => false
  }
})

let incrCount = increases->Belt.Array.length

Js.log3("Part 2: ", increases, incrCount)
