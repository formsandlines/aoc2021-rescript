open Belt
open Helper

let data = Input.read("./src/data/input_day13.txt")

let ex1 = "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"


let processInput = input => {
  let (folds, coords) = input->Input.toLines
    ->Array.partition((line => line->Js.String2.startsWith("fold")))

  let folds = folds->Array.map(line => switch line->Js.String2.trim
    ->Js.String2.split(" ")->ArrayExt.filterEmptyStr {
      | [_,_,instr] => switch instr->Js.String2.split("=") {
          | [axis,coord] => (axis, coord->Int.fromString->Option.getExn)
          | _ => raise(Not_found)
          }
      | _ => raise(Not_found)
      })

  let coords = coords->Array.map(line => switch line->Js.String2.trim
    ->Js.String2.split(",")->ArrayExt.filterEmptyStr
    ->ArrayExt.toIntArrExn {
      | [a,b] => (a,b)
      | _ => raise(Not_found)
      })

  (folds, coords)
}

let (folds, coords) = data->processInput

/* Js.log2(coords, folds) */


// Part 1 *

let foldAll = (coords, instr) => {

  let dist = (a,b) => Js.Math.abs_int( b - a )

  let applyFold = ((x,y), instr) => switch instr {
    | ("x", axisX) => (axisX - dist(axisX, x), y)
    | ("y", axisY) => (x, axisY - dist(axisY, y))
    | _ => raise(Not_found)
    }

  coords->Array.map(coord => coord->applyFold(instr))
    ->Set.fromArray(~id=module(Tuple.CmpInt))->Set.toArray
}


let coordsFolded = coords->foldAll( folds->Array.getExn(0) )

Js.log2("First fold dot count: ", coordsFolded->Array.length)


// Part 2 *

let coordsFoldedComplete = folds->Array.reduce(coords, (coordsFolded, fold) =>
  coordsFolded->foldAll(fold) )

let renderGrid = coords => {
  let (maxX, maxY) = switch coords->Array.unzip {
    | (xs, ys) => (xs->Js.Math.maxMany_int, ys->Js.Math.maxMany_int)
    }

  Array.range(0,maxY)->Array.forEach(row => Js.log(
    Array.range(0,maxX)->Array.reduce("", (str, col) => str ++
      if coords->Array.some(((x,y)) => x == col && y == row) { `#` } else { `.` }
    )) )
}

Js.log("Code:")
coordsFoldedComplete->renderGrid
