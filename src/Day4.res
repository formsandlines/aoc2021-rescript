let raw = Node_fs.readFileSync("./src/data/input_day4.txt", #utf8)

let sample = `7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7`

type board = { rows: array<array<int>>, cols: array<array<int>> }
let (rowN, colN) = (5, 5)

let (drawNums, boards) = {
  let data = raw->Js.String2.split("\n")

  let drawNums = data->Belt.Array.getUnsafe(0)
    ->Js.String2.split(",")
    ->Belt.Array.map(str => str
      ->Belt.Int.fromString->Belt.Option.getUnsafe)
    ->Belt.List.fromArray

  let dataRows = data
    ->Belt.Array.slice(~offset=2, ~len=data->Belt.Array.length-2)
    ->Belt.Array.keep(str => str != "")
    ->Belt.Array.map(str => str
      ->Js.String2.split(" ")
      ->Belt.Array.keep(str => str != "")
      ->Belt.Array.map(str => str
        ->Belt.Int.fromString
        ->Belt.Option.getUnsafe))
  
  let boards = Belt.Array.makeBy((dataRows->Belt.Array.length / rowN), (i) => {
    let rows = dataRows->Belt.Array.slice(~offset=(i * rowN), ~len=rowN)
    let cols = Belt.Array.makeBy(colN, (j) => rows
      ->Belt.Array.map(row => row->Belt.Array.getUnsafe(j)))
    { rows, cols }
  })

  (drawNums, boards)
}

Js.log2("Total draws: ", drawNums->Belt.List.length)


// Part 1 *

let decideWin = (arr, draws) => arr
  ->Belt.Array.every(n => draws->Belt.List.has(n, \"=="))

let unmarkedSum = (rows, draws) => rows
  ->Belt.Array.reduce(0, (sum, row) => sum + row
    ->Belt.Array.keep(n => !(draws->Belt.List.has(n, \"==")))
    ->Belt.Array.reduce(0, \"+"))

let playBoard = (board, drawNums) => {
  let rec aux = (nextDraws, draws) =>
    switch nextDraws {
      | list{} => (false, 0, draws)
      | list{n, ...rest} => {
          let draws = draws->Belt.List.add(n)
          let wins =
               board.rows->Belt.Array.some(row => decideWin(row, draws))
            || board.cols->Belt.Array.some(col => decideWin(col, draws))

          if wins { (true, unmarkedSum(board.rows, draws) * n, draws) }
          else { rest->aux(draws) }
        }
      }

  drawNums->aux(list{})
}

let leaderBoard = boards->Belt.Array.map(board => {
  let (wins, score, draws) = board->playBoard(drawNums)
  let drawCount = draws->Belt.List.length
  (drawCount, { "wins": wins, "score": score })
})->Belt.Map.Int.fromArray

Js.log2("Leaderboard: ", leaderBoard->Belt.Map.Int.toArray)


let firstPlace = leaderBoard->Belt.Map.Int.minimum->Belt.Option.getUnsafe

Js.log2("First place: ", firstPlace)


// Part 2 *

let lastPlace = leaderBoard->Belt.Map.Int.maximum->Belt.Option.getUnsafe

Js.log2("Last place: ", lastPlace)




