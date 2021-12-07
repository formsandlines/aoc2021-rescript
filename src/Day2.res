let raw = Node_fs.readFileSync("./src/data/input_day2.txt", #utf8)

let sample = "forward 5
down 5
forward 8
up 3
down 8
forward 2"

type instr = Forward(int) | Down(int) | Up(int)

let makeInstr = (dirStr, val) => switch dirStr {
  | "forward" => Some(Forward(val))
  | "down" => Some(Down(val))
  | "up" => Some(Up(val))
  | _ => None
}

let data = raw->Js.String2.split(`\n`)
  ->Belt.Array.keep(str => str != "")
  ->Belt.Array.mapU((. str) =>
    switch str->Js.String2.split(" ") {
      | [str, n] => makeInstr(str, n->Belt.Int.fromString->Belt.Option.getExn)
      | _ => None
    }->Belt.Option.getExn
  )->Belt.List.fromArray


// Part 1 *

type pos = { horiz: int, depth: int }

let rec runInstr = (data, pos) => switch data {
  | list{} => pos
  | list{Forward(n), ...rest} => rest->runInstr({...pos, horiz: pos.horiz + n})
  | list{Down(n), ...rest} => rest->runInstr({...pos, depth: pos.depth + n})
  | list{Up(n), ...rest} => rest->runInstr({...pos, depth: pos.depth - n})
}

let newPos = data->runInstr({ horiz: 0, depth: 0 })
let result = newPos.horiz * newPos.depth

Js.log2(newPos, result)


// Part 2 *

type pos' = { horiz: int, depth: int, aim: int }

let rec runInstr = (data, pos) => switch data {
  | list{} => pos
  | list{Forward(n), ...rest} => rest->runInstr({...pos,
      horiz: pos.horiz + n,
      depth: pos.depth + pos.aim * n })
  | list{Down(n), ...rest} => rest->runInstr({...pos,
      aim: pos.aim + n})
  | list{Up(n), ...rest} => rest->runInstr({...pos,
      aim: pos.aim - n})
}

let newPos = data->runInstr({ horiz: 0, depth: 0, aim: 0 })
let result = newPos.horiz * newPos.depth

Js.log2(newPos, result)
