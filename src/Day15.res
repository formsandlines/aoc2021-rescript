open Belt
open Helper

let processInput = input => {
  input->Input.toLines
    ->Array.map(line => line->Js.String2.split("")
      ->ArrayExt.toIntArrExn)
}


let getAdjacentRisks = ((x,y), riskMap) => {
  let adjacent = [
            (x,y-1),          
    (x-1,y),        (x+1,y),
            (x,y+1)]

  adjacent->Array.keepMap(((x',y')) =>
    riskMap->Array.get(y')->Option.flatMap(row => row->Array.get(x')))
}

let findCheapestPath = ((x,y), riskMap) => {

  /* let riskFrom = riskMap->Map.get((x,y)) */

  let row = riskMap->Array.getExn(y)
  let riskFrom = row->Array.getExn(x)

  /* (x,y)->getAdjacentRisks(riskMap) */

  

}


let solve1 = input => {

  /* let riskMap = input->Array.mapWithIndex((y, line) => line */
  /*   ->Array.mapWithIndex((x, riskLvl) => ((x,y),riskLvl) )) */
  /*   ->Array.concatMany->Map.fromArray(~id=module(Tuple.CmpInt)) */

  let test = findCheapestPath((0,0), input)



  /* Js.log(riskMap->Map.toArray) */
  Js.log(test)
}


let data = Input.read("./src/data/input_day15.txt")

let ex1 = 
"1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"


let input = ex1->processInput

solve1(input)
