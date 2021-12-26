open Belt
open Helper


/** Identifies digits by their intersections of segments with other digits */
let identify = (context, compDigit, compSize) => {
  let (digit, remaining) = context
    ->Array.partition(set => Set.String.intersect(compDigit,set)
      ->Set.String.size == compSize)
  (digit->Array.getExn(0), remaining)
}

/** Performs set operations on segment-sets to deduce each digit */
let decodeDigits = (encrDigitSets, uniqueSegmDigits) => {

  // first, get all segment sets for digits with unique segment counts
  let (d1,d4,d7,d8) = { 
    // transform map of unique segment digits to map (digit |-> set of segments)
    let digits = uniqueSegmDigits->Map.Int.toArray
      ->Array.map(((segmCount,digit)) => {
        ( digit, // now key, not value!
          encrDigitSets->Array.keep(set => set->Set.String.size == segmCount)
            ->Array.getExn(0) // segment count must be unique by definition
        )
      })->Map.Int.fromArray

    ( digits->Map.Int.getExn(1), digits->Map.Int.getExn(4),
      digits->Map.Int.getExn(7), digits->Map.Int.getExn(8) )
  }
  // the rest of the digits have either 5 or 6 segments, so we isolate them
  let seg5s = encrDigitSets->Array.keep(set => (set->Set.String.size == 5))
  let seg6s = encrDigitSets->Array.keep(set => (set->Set.String.size == 6))

  // now we can identify the remaining digits from what we already know
  // by comparing the number of segment intersections between digits
  let (d3,seg5s) = seg5s->identify(d1,2)
  let (d9,seg6s) = seg6s->identify(d3,5)
  let (d5,seg5s) = seg5s->identify(d9,5)
  let d2 = seg5s->Array.getExn(0)
  let (d6,seg6s) = seg6s->identify(d7,2)
  let d0 = seg6s->Array.getExn(0)

  Map.fromArray([
    (d0,0),(d1,1),(d2,2),(d3,3),(d4,4),(d5,5),(d6,6),(d7,7),(d8,8),(d9,9) ],
  ~id=module(SetExt.CmpStr))
}


let part1 = (input, uniqueSegmDigits) => { // *

  // map each output digit to its length, flatten array of lenghts
  let outputSegmCounts = input->Array.map(line => switch line {
    | [_, output] => output->Array.map(str => str->Js.String2.length)
    | _ => raise(Not_found)
  })->Array.concatMany

  // sum up digit lengths that correspont to unique (1-element) values
  let uniqueCountSum = outputSegmCounts->Array.reduce(0, (sum, segmCount) => sum +
    (uniqueSegmDigits->Map.Int.has(segmCount) ? 1 : 0) )

  Js.log2("Sum of digits with unique segment count in output:", uniqueCountSum)
}


let part2 = (input, uniqueSegmDigits) => { // *

  // for all lines, create arrays of segment-sets from each digit
  let inputSegmentSets = input->Array.map(line => switch line {
    | [encryptedDigits, _] => encryptedDigits->
        Array.map(encrStr => encrStr->Js.String2.split("")
          ->Set.String.fromArray)
    | _ => raise(Not_found)
  })

  // reduce each array of sets to a map (segments |-> determined digit)
  let digitMaps = inputSegmentSets 
    ->Array.map(segmSetArr => segmSetArr->decodeDigits(uniqueSegmDigits))

  // decode outputs by matching their numbers to corresponding segments in the map
  let displays = Array.zipBy(digitMaps, input, (digitMap, line) => switch line {
    | [_, output] => output->Array.map(encrStr => {
          let encrSet = encrStr->Js.String2.split("")->Set.String.fromArray
          switch digitMap->Map.get(encrSet) {
            | Some(n) => n->Int.toString
            | None => "_"
            }
        })->Js.Array2.joinWith("")
    | _ => raise(Not_found)
  })

  let outputVals = displays->Array.map(nStr => nStr
    ->Int.fromString->Option.getWithDefault(-99999))

  /* Js.log2("Digit maps:",digitMaps->Array.map(map => map */
  /*   ->Map.toArray->Array.map(((k,v)) => */ 
  /*     (k->Set.String.toArray->Js.Array2.joinWith(""),v))) ) */

  Js.log2("Output numbers:",displays)

  Js.log2("Output values:",outputVals->Array.reduce(0, \"+"))
}


let data = Input.read("./src/data/input_day8.txt")

let ex1 = `be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce`

let ex2 = `acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf`

let processInput = input => input->Input.toLines
  ->Array.map(line => line->Js.String2.split("|")
  ->Array.map(str => str->Js.String2.split(" ")->ArrayExt.filterEmptyStr))

let input = data->processInput

// make maps from each segment count |-> all digits with that segment count
// -> separate maps for unique and equal segment counts
let (uniqueSegmDigits, equalSegmDigits) = (
  Map.Int.fromArray([ (2,1), (3,7), (4,4), (7,8) ]),
  Map.Int.fromArray([ (5,[2,3,5]), (6,[0,6,9]) ]) // <- just for reference
)

ignore( input->part1(uniqueSegmDigits) )
ignore( input->part2(uniqueSegmDigits) )


