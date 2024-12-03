open Belt
open Helper


let processInput = input => {
  input->Input.toLines
}


let solve1 = input => {
  Js.log(input)
}


let data = Input.read("./src/data/input_day19.txt")

let ex1 =
"--- scanner 0 ---
0,2
4,1
3,3

--- scanner 1 ---
-1,-1
-5,0
-2,1"

let input = ex1->processInput

solve1(input)


