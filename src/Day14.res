open Belt
open Helper


let processInput = input => {
  let lines = input->Input.toLines
  
  let templ = lines->Array.getExn(0)
    ->Js.String2.split("")->ArrayExt.filterEmptyStr->List.fromArray
  
  let rules = lines->Js.Array2.sliceFrom(1)
    ->Array.map(line => switch line->Js.String2.split(" -> ") {
      | [pair, elem] => ((pair->Js.String2.get(0), pair->Js.String2.get(1)), elem)
      | _ => raise(Not_found)
      })
      ->Map.fromArray(~id=module(Tuple.CmpStr))

  (templ, rules)
}

let applyRule = (a, b, rules) => 
  rules->Map.get((a,b))

let polymerize = (templ, rules) => {
  let rec aux = (templ, prevElem, polymer) => switch templ {
    | list{} => polymer
    | list{elem, ...rest} => {
          let polymer = switch applyRule(prevElem, elem, rules) {
            | Some(x) => polymer->List.add(x)->List.add(elem)
            | None => polymer->List.add(elem)
            }
          rest->aux(elem, polymer)
        }
    }

  let firstElem = templ->List.headExn
  /* list{firstElem, ...templ->List.tailExn->aux(firstElem)} */
  templ->List.tailExn->aux(firstElem, list{firstElem})
    ->List.reverse
}

let polymerize_fast = (templ, rules) => {


}

let summarize = polymer => {
  let types = polymer->List.toArray->Set.String.fromArray

  types->Set.String.reduce(Map.String.empty, (stats, tp) =>
    stats->Map.String.set(tp, polymer->List.keep(elem => elem == tp)->List.length) )
}

let getStats = summary => summary->Map.String.valuesToArray
  ->Array.reduce((Js.Int.max,0), ((least,most), sum) =>
    ( Js.Math.min_int(least,sum), Js.Math.max_int(most,sum) ))


let solve1 = (templ, rules) => { // *

  let polymer = Array.range(1,10)->Array.reduce(templ, (polymer, _) =>
    polymer->polymerize(rules) )

  let summary = polymer->summarize
  let (leastCommon, mostCommon) = summary->getStats

  /* Js.log2("Sequence: ", polymer->List.toArray->Js.Array2.joinWith("")) */

  Js.log(summary->Map.String.toArray)

  Js.log2("Least common element: ", leastCommon)
  Js.log2("Most common element: ", mostCommon)

  Js.log2("Solution: ", mostCommon - leastCommon)
}


let solve2 = (templ, rules) => {

  let polymer = Array.range(1,20)->Array.reduce(templ, (polymer, _) =>
    polymer->polymerize(rules) )

  let summary = polymer->summarize
  let (leastCommon, mostCommon) = summary->getStats

  /* Js.log2("Sequence: ", polymer->List.toArray->Js.Array2.joinWith("")) */

  Js.log(summary->Map.String.toArray)

  Js.log2("Least common element: ", leastCommon)
  Js.log2("Most common element: ", mostCommon)

  Js.log2("Solution: ", mostCommon - leastCommon)
}

let data = Input.read("./src/data/input_day14.txt")

let ex1 = 
"NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"

let (polymerTempl, pairInsertRules) = data->processInput

Js.log2(polymerTempl->List.toArray, pairInsertRules->Map.toArray)

solve1(polymerTempl, pairInsertRules)

solve2(polymerTempl, pairInsertRules) // <- "JavaScript heap out of memory"!

// Note: Only chunking might help for the 2. task



