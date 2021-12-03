let raw = Node_fs.readFileSync("./src/data/input_day3.txt", #utf8)

let sample = "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"

let data = raw->Js.String2.split("\n")
  ->Belt.Array.keep(str => str !== "")
  ->Belt.Array.map(str => str->Js.String2.split("")
    ->Belt.Array.map(ch => ch->Belt.Int.fromString->Belt.Option.getExn))

let dataLen = data->Belt.Array.length

let countColumnBits = (data, startCol, colSpan) => {
  switch data->Belt.Array.get(0) {
    | Some(arr) => {
      let colN = arr->Belt.Array.length
      let sumCol = Belt.Array.makeBy(colN, i =>
        i >= startCol && i < (startCol + colSpan) ? Some(0) : None)

      Some(data->Belt.Array.reduce(sumCol,
        (counts, bitArr) => if bitArr->Belt.Array.length === colN {
            bitArr->Belt.Array.zipBy(counts, (n1, maybe_n2) =>
              maybe_n2->Belt.Option.map(n2 => n1 + n2))
          } else { counts })
          ->Belt.Array.keepMap(opt => opt))
      }
    | None => None
  }
}

let counts = data->countColumnBits(0, data[0]->Belt.Array.length)
  ->Belt.Option.getExn


// Part 1 *

let gammaRate = counts
  ->Belt.Array.reduce("0b", (binStr, n) => binStr ++ (n > dataLen/2 ? "1" : "0"))
  ->Js.Float.fromString

let epsilonRate = counts
  ->Belt.Array.reduce("0b", (binStr, n) => binStr ++ (n > dataLen/2 ? "0" : "1"))
  ->Js.Float.fromString

let result = gammaRate *. epsilonRate

Js.log3(gammaRate, epsilonRate, result)


// Part 2 *

let convergeByColVal = (arr, getColVal) => {
  let filterByNthVal = (arr, index, keepVal) =>
    arr->Belt.Array.keep(vals => vals[index] === keepVal)

  let arrLen = arr[0]->Belt.Array.length

  let rec aux = (arr, index) => {
    if (index < arrLen) && (arr->Belt.Array.length > 1) { 
      let colVal = getColVal(arr, index)
      let selArr = arr->filterByNthVal(index, colVal)

      selArr->aux(index + 1)
    }
    else { arr[0] }
  }
  arr->aux(0)
}

let count0sAnd1s = (arr, index) => {
  let all = arr->Belt.Array.length
  if (all > 0) {
    let ones = arr->countColumnBits(index, 1)
      ->Belt.Option.flatMap(counts => counts
        ->Belt.Array.get(0))->Belt.Option.getWithDefault(0)
    let zeros = all - ones
    (ones, zeros)
  }
  else { (0, 0) }
}

let oxygenGeneratorRating = data->convergeByColVal(
  (arr, index) => {
    let (ones, zeros) = count0sAnd1s(arr, index)
    ones >= zeros ? 1 : 0
  })

let co2ScrubberRating = data->convergeByColVal(
  (arr, index) => {
    let (ones, zeros) = count0sAnd1s(arr, index)
    ones >= zeros ? 0 : 1
  })

let result = {
  let toDecimal = arr => ("0b" ++ arr->Js.Array2.joinWith(""))
    ->Js.Float.fromString

  oxygenGeneratorRating->toDecimal *. co2ScrubberRating->toDecimal
}

Js.log3(oxygenGeneratorRating, co2ScrubberRating, result)


