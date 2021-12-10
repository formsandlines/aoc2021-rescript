open Belt

module ArrayExt = {

  let filterEmptyStr = strArr => strArr
    ->Array.keep(str => str != "")

  let toIntArrExn = strArr => strArr
    ->Array.map(nStr => nStr->Int.fromString->Option.getExn)
}

module Input = {

  let read = path => Node_fs.readFileSync(path, #utf8)

  let toLines = str => str->Js.String2.split("\n")
    ->ArrayExt.filterEmptyStr

}

module BigInt = {
  type t

  @val
  external make: int => t = "BigInt"

  let cmp: (t,t) => int = %raw(`function(a,b) {
    if (a < b) { return -1; }
    else if (a > b) { return 1; }
    else { return 0; }
  }`)

  let add: (t,t) => t = %raw(`function(x,y) { return x + y; }`)
  let addInt = (t,i) => add(t, make(i))

  let mul: (t,t) => t = %raw(`function(x,y) { return x * y; }`)
  let mulInt = (t,i) => mul(t, make(i))

  let eq: (t,t) => bool = %raw(`function(x,y) { return x == y; }`)
  let gr: (t,t) => bool = %raw(`function(x,y) { return x > y; }`)
  let sm: (t,t) => bool = %raw(`function(x,y) { return x < y; }`)
  let grEq: (t,t) => bool = %raw(`function(x,y) { return x >= y; }`)
  let smEq: (t,t) => bool = %raw(`function(x,y) { return x <= y; }`)

  let zero: t = %raw(`0n`)
  let one: t = %raw(`1n`)
}
