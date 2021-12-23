open Belt
open Helper

module SFNum = {

  type rec t = Num(int) | Pair(t,t)

  let rec show = sfn => switch sfn {
    | Num(n) => n->Int.toString
    | Pair(a,b) => `[${a->show},${b->show}]`
    }

  module Syntax = {
    exception ParseError(string)

    let isDigit = ch => switch ch {
      | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => true
      | _ => false
      }

    let rec parseExpr = (stream, expr) => switch stream {
      | list{} => raise(ParseError("Expected ']'"))
      | list{"]", ...r} => switch expr {
          | [a,b] => (Pair(a,b), r)
          | _ => raise(ParseError("Bad pair syntax"))
          }
      | list{"[", ...r} => {
          let (innerExpr, r') = r->parseExpr([])
          r'->parseExpr( expr->Array.concat([innerExpr]) )
        }
      | list{",", ...r} => r->parseExpr(expr)
      | list{d, ...r} if d->isDigit => {
          let (num, r') = r->parseNum(d)
          r'->parseExpr( expr->Array.concat([num]) )
        }
      | _ => raise(ParseError("Expected ']'"))
      }
    and parseNum = (stream, nStr) => switch stream {
      | list{d, ...r} if d->isDigit => r->parseNum(nStr ++ d)
      | _ => switch nStr->Int.fromString {
          | Some(n) => (Num(n), stream)
          | None => raise(ParseError("Bad number syntax"))
          }
      }

    let parse = stream => switch stream->List.tailExn
      ->parseExpr([]) {
        | (sfn, list{}) => sfn
        | _ => raise(ParseError("Parsing failed"))
        }
  }

  let read = str => str
    ->Js.String2.split("")->List.fromArray
    ->Syntax.parse
    

  let findLeftmostNumPair = (~minDepth=0, sfn) => {

    let rec aux = (sfn, idx) => switch sfn {
      | Pair(Num(n),Num(m)) =>
          if idx->List.length >= minDepth {
            Some({"idx": idx, "pair": (n,m)})
          } else { None }
      | Pair(a,b) =>
          switch a->aux(idx->List.add(0)) {
            | Some(result) => Some(result)
            | None => b->aux(idx->List.add(1))
            }
      | Num(_) => None
      }

    sfn->aux(list{})
  }


  let findAdjacentNum = (~findLeft, sfn, compIdx) => {

    let rec aux = (sfn, idx, adjPath) => switch sfn {
      | Num(n) => Some({"idx": idx, "num": n})
      | Pair(a,b) => switch adjPath {
        | list{} => if findLeft {
            b->aux(idx->List.add(1), list{})
          } else {
            a->aux(idx->List.add(0), list{})
          }
        | list{0, ...r} => a->aux(idx->List.add(0), r)
        | list{_, ...r} => b->aux(idx->List.add(1), r)
        }
      }

    // build adjacent path by finding the last (first in idx) change
    // from the target direction (0=left, 1=right)
    let adjPath = {
      let rec fn = idx => switch idx {
        | list{} => list{}
        | list{0, ...r} if !findLeft => list{1, ...r}
        | list{1, ...r} if  findLeft => list{0, ...r}
        | list{_, ...r} => r->fn
        }
      compIdx->fn
    }

    if adjPath->List.length == 0 {
      None // no change -> no adjacent path
    } else {
      // follow adjacent path and afterwards follow opposite direction
      sfn->aux(list{}, adjPath->List.reverse)
    }
  }

  let map = (sfn, fn) => {
    let rec aux = (sfn, idx) => switch sfn {
      | Num(n) => Num(fn(idx, n))
      | Pair(a,b) => Pair(a->aux(idx->List.add(0)), b->aux(idx->List.add(1)))
      }

    sfn->aux(list{})
  }

  let flatMap = (sfn, fn) => {
    let rec aux = (sfn, idx) => switch sfn {
      | Num(n) => fn(idx, Num(n))
      | Pair(a,b) => fn(idx,
          Pair(a->aux(idx->List.add(0)), b->aux(idx->List.add(1))) )
      }

    sfn->aux(list{})
  }

  let reduce = (sfn, init, reducerFn) => {
    
    let reducePair = ((a,b), init, fn, idx) => {
      let acc = fn(init, a, idx->List.add(0))
      fn(acc, b, idx->List.add(1))
    }

    let rec aux = (acc, sfn, idx) => {
      let acc = reducerFn(acc, sfn, idx)
      switch sfn {
        | Pair(a,b) => (a,b)->reducePair(acc, aux, idx)
        | _ => acc
        }
    }

    aux(init, sfn, list{})
  }

  let explode = sfn => {

    switch sfn->findLeftmostNumPair(~minDepth=4) {
      | Some(toExpl) => {
          let leftAdj = sfn->findAdjacentNum(~findLeft=true, toExpl["idx"])
          let rightAdj = sfn->findAdjacentNum(~findLeft=false, toExpl["idx"])

          let (leftVal, rightVal) = toExpl["pair"]

          let sfn = sfn->flatMap((idx, sfn) => switch sfn {
            | Num(n) => Num(switch leftAdj {
                | Some(adj) if adj["idx"] == idx => n + leftVal
                | _ => switch rightAdj {
                  | Some(adj) if adj["idx"] == idx => n + rightVal
                  | _ => n
                  }
                })
            | Pair(_,_) => if idx == toExpl["idx"] { Num(0) } else { sfn }
            })

          (sfn, true)
        }
      | None => (sfn, false)
      }
  }

  let split = sfn => {

    let rec aux = (sfn, hasSplit) => switch sfn {
      | Num(n) => if !hasSplit && n >= 10 {
          let n_half = n->Int.toFloat /. 2.0
          let (a,b) = (Num(n_half->Js.Math.floor_int),
                       Num(n_half->Js.Math.ceil_int))
          (Pair(a,b), true)
        } else { (Num(n), hasSplit) }
      | Pair(a,b) => if !hasSplit {
          let (a', hasSplit) = a->aux(hasSplit)
          let (b', hasSplit) = hasSplit ? (b, hasSplit) : b->aux(hasSplit)
          (Pair(a',b'), hasSplit)
        } else { (Pair(a,b), hasSplit) }
      }
    sfn->aux(false)
  }

  let add = (sfn_a, sfn_b) => {
    
    let rec reduceSum = sfn => {
      let (sfn, hasExpl) = sfn->explode

      let (sfn, hasSplit) = if !hasExpl {
        let (sfn, hasSplit) = sfn->split
        (sfn, hasSplit)
      } else { (sfn, false) }

      if hasExpl || hasSplit { sfn->reduceSum } else { sfn }
    }

    Pair(sfn_a, sfn_b)->reduceSum
  }

  let rec magnitude = sfn => switch sfn {
    | Num(n) => n
    | Pair(a,b) => (a->magnitude * 3) + (b->magnitude * 2)
    }

  module Test = {

    let testFlatmap = () => {

      let test = (label, (input, mapIdx), expected) => {
        Js.log2(label, input)

        let result = input->read->flatMap((idx, sfn) =>
          if idx == mapIdx { Num(0) } else { sfn })

        Js.log2("Correct:", result->show == expected)
      }

      test("Flatmap test 1", ("[[1,2],[[3,4],5]]", list{0,1}),
           "[[1,2],[0,5]]")
    }

    let testReduce = () => {

      let test = (label, input, expected) => {
        Js.log2(label, input)

        let result = input->read->reduce(0, (acc, sfn, _) => acc + switch sfn {
          | Num(n) => n
          | _ => 0
        })

        Js.log2("Correct:", result == expected)
      }

      test("Reduce test 1", "[[1,2],[[3,4],5]]", 15)
    }

    let testExplode = () => {
      let test = (label, input, expected) => {
        Js.log2(label, input)

        let (result,hasExpl) = input->read->explode
        Js.log2("Exploded:", hasExpl)

        let result = result->show
        Js.log2("Correct:", result == expected)
      }

      test("Explode test 1", "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]",
           "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]")

      test("Explode test 2", "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]",
           "[[[[0,7],4],[15,[0,13]]],[1,1]]")
           
      test("Explode test 3", "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]",
           "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")

      test("Explode test 4", "[[[[[9,8],1],2],3],4]",
           "[[[[0,9],2],3],4]")

      test("Explode test 5", "[7,[6,[5,[4,[3,2]]]]]",
           "[7,[6,[5,[7,0]]]]")

      test("Explode test 6", "[[6,[5,[4,[3,2]]]],1]",
           "[[6,[5,[7,0]]],3]")

      test("Explode test 7", "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]",
           "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")

      test("Explode test 8", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]",
           "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
    }

    let testSplit = () => {
      let test = (label, input, expected) => {
        Js.log2(label, input)

        let (result,hasSplit) = input->read->split
        Js.log2("Splitted:", hasSplit)

        let result = result->show
        Js.log2("Correct:", result == expected)
      }

      test("Split test 1", "[[[[0,7],4],[15,[0,13]]],[1,1]]",
           "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")

      test("Split test 2", "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]",
           "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]")
    }

    let testAdd = () => {
      let test = (label, (input_a, input_b), expected) => {
        Js.log(label)

        let (sfn_a, sfn_b) = (input_a->read, input_b->read)
        let result = add(sfn_a, sfn_b)->show

        Js.log2("Expected:", expected)
        Js.log2("Result:", result)

        Js.log2("Correct:", result == expected)
      }

      test("Add test 1", ("[[[[4,3],4],4],[7,[[8,4],9]]]", "[1,1]"),
           "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")

      test("Add test 2", ("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
                          "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"),
           "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]")
 
      test("Add test 3", ("[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]",
                          "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"),
           "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]")

      test("Add test 4", ("[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]",
                          "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"),
           "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]")

      test("Add test 5", ("[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]",
                          "[7,[5,[[3,8],[1,4]]]]"),
           "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]")

      test("Add test 6", ("[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]",
                          "[[2,[2,2]],[8,[8,1]]]"),
           "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]")

      test("Add test 7", ("[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]",
                          "[2,9]"),
           "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]")
    }

  }
}


let solve1 = input => { // *
  let input = input->List.fromArray

  let sum = input->List.tailExn
    ->List.reduce(input->List.headExn, (sum, sfn) => SFNum.add(sum, sfn))

  Js.log2(sum->SFNum.show, sum->SFNum.magnitude)

}

let solve2 = input => { // *

  let candidates = input->ArrayExt.cartesProd(2)

  let largestMag = candidates->Array.reduce(0, (largest, cand) => {
      let mag = switch cand {
        | [sfn_a, sfn_b] => SFNum.add(sfn_a, sfn_b)->SFNum.magnitude
        | _ => -1
        }
      if mag > largest { mag } else { largest }
    })

  Js.log(largestMag)

}


let processInput = input => input->Input.toLines
    ->Array.map(line => line->SFNum.read)

let data = Input.read("./src/data/input_day18.txt")

let ex1 =
"[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"

let input = data->processInput

/* ignore(SFNum.Test.testFlatmap()) */
/* ignore(SFNum.Test.testReduce()) */
/* ignore(SFNum.Test.testSplit()) */
/* ignore(SFNum.Test.testExplode()) */
/* ignore(SFNum.Test.testAdd()) */

solve1(input)
solve2(input)
