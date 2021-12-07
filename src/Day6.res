open Belt

let raw = Node_fs.readFileSync("./src/data/input_day6.txt", #utf8)

let sample = `3,4,3,1,2`

module Lanternfish = {
  type t = { birthTimer: int }

  let firstBirthPeriod = 8
  let nextBirthPeriod = 6

  let make = (~birthPeriod=?, ()) => switch birthPeriod {
    | None => { birthTimer: firstBirthPeriod }
    | Some(days) => { birthTimer: days }
    }

  let show = fish => fish.birthTimer
}

// Part 1 *

let rec simulatePopulation = (generations, days) => {
  let rec advanceGen = (currGen: list<Lanternfish.t>, (births, nextGen)) => switch currGen {
    | list{} => List.concat(births, nextGen->List.reverse)
    | list{fish, ...rest} =>
        if fish.birthTimer > 0 {
          let fish: Lanternfish.t = {birthTimer: fish.birthTimer - 1}
          rest->advanceGen((births, nextGen->List.add(fish)))
        } else {
          let fish: Lanternfish.t = {birthTimer: Lanternfish.nextBirthPeriod}
          rest->advanceGen((births->List.add(Lanternfish.make()), nextGen->List.add(fish)))
        }
    }

  switch generations->List.head {
    | Some(currGen) if days > 0 => {
        let nextGen = currGen->advanceGen((list{}, list{}))
        generations->List.add(nextGen)->simulatePopulation(days - 1)
      }
    | _ => generations
  }
}

let initGen = raw->Js.String2.split(",")->Array.map(nStr => Lanternfish.make(
    ~birthPeriod = nStr->Int.fromString->Option.getUnsafe, ()))
  ->Array.reverse->List.fromArray

let days = 80
let generations = list{initGen}->simulatePopulation(days)

/* generations->List.reverse->List.forEachWithIndex((i, gen) => */
/*   Js.log3(i, "days:",
      gen->List.toArray->Array.reverse->Array.map(f => f->Lanternfish.show) )) */

Js.log2(`Total fish after ${days->Int.toString} days:`,
        generations->List.head->Option.getUnsafe->List.length)


// Part 2 *

module Js_bigInt = {
  type t
  let sum: (t,t) => t = %raw(`function(x,y) { return x + y; }`)
  let fromInt: int => t = %raw(`function(x) { return BigInt(x); }`)
  let zero: t = %raw(`0n`)
  let one: t = %raw(`1n`)
}

let rec calcBirths = (birthCount, days) => {
  let cycleDays = Lanternfish.nextBirthPeriod + 1

  let daysNextBirths = days - (birthCount+1)
  let births = 1 + daysNextBirths/cycleDays

  let remainingDaysOffspring = Array.makeBy(births, i =>
    days - ((birthCount+1) + i * cycleDays))

  let allDescBirths = remainingDaysOffspring
    ->Array.reduce(Js_bigInt.zero, (descendants, remDays) => {
      let descBirths = if remDays > Lanternfish.firstBirthPeriod
        { calcBirths(Lanternfish.firstBirthPeriod, remDays) } else { Js_bigInt.zero }
      Js_bigInt.sum(descendants, descBirths)
    })

  Js_bigInt.sum(Js_bigInt.fromInt(births), allDescBirths)
}


let days = 256

let initGen = raw->Js.String2.split(",")
  ->Array.map(nStr => nStr->Int.fromString->Option.getUnsafe)

// CAUTION: performing all calculations might take 10-20 min.!
let birthCountResults = Array.makeBy(5, i => 
  Js_bigInt.sum(calcBirths(i+1, days), Js_bigInt.one))

/* Because calculations can take a couple of minutes, I hard-coded the results
   I got back from the following logs: */

/* Js.log( Js_bigInt.sum(calcBirths(1, days), Js_bigInt.one) ) */
/* Js.log( Js_bigInt.sum(calcBirths(2, days), Js_bigInt.one) ) */
/* Js.log( Js_bigInt.sum(calcBirths(3, days), Js_bigInt.one) ) */
/* Js.log( Js_bigInt.sum(calcBirths(4, days), Js_bigInt.one) ) */
/* Js.log( Js_bigInt.sum(calcBirths(5, days), Js_bigInt.one) ) */

// 256 days:
/* let birthCountResults: array<Js_bigInt.t> = [ */
/*   %raw(`6206821033n`), */
/*   %raw(`5617089148n`), */
/*   %raw(`5217223242n`), */
/*   %raw(`4726100874n`), */
/*   %raw(`4368232009n`) */
/* ] */

let populationSize = initGen->Array.reduce(Js_bigInt.zero, (sum, birthCount) => {
  Js_bigInt.sum(sum, birthCountResults->Array.getUnsafe(birthCount-1)) })

Js.log2(`Total fish after ${days->Int.toString} days:`, populationSize)
