@module("./data/day1_data.js") external raw: string = "data"

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

let data = raw->Js.String2.split(`\n`)
  ->Belt.Array.mapU((. str) => str->Belt.Int.fromString->Belt.Option.getExn)

let increases = data->Belt.Array.keepWithIndexU((. n,i) => {
  switch data->Belt.Array.get(i-1) {
    | Some(nPrev) => n > nPrev
    | None => false
  }
})

let incrCount = increases->Belt.Array.length

Js.log2(increases, incrCount)
