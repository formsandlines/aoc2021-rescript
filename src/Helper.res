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

module Tuple = {
  let has = elem => ( ((a,b)) => a == elem || b == elem )
  let hasNot = elem => ( ((a,b)) => a != elem && b != elem )

  module CmpInt =
    Belt.Id.MakeComparable({
      type t = (int, int)
      let cmp = ((a0, a1), (b0, b1)) =>
        switch (Pervasives.compare(a0, b0)) {
        | 0 => Pervasives.compare(a1, b1)
        | c => c
        }
    })

  module CmpStr =
    Belt.Id.MakeComparable({
      type t = (string, string)
      let cmp = ((a0, a1), (b0, b1)) =>
        switch (Pervasives.compare(a0, b0)) {
        | 0 => Pervasives.compare(a1, b1)
        | c => c
        }
    })
}

module Tree = {
  type rec t<'a> = Leaf('a) | Branch(array<t<'a>>)

  let show = tree => {
    let rec aux = (tree, d, i) => {
      let indent = n => `  `->Js.String2.repeat(n)

      switch tree {
      | Leaf(str) => {
          let idx = `<${d->Int.toString},${i->Int.toString}>`
          str ++ " " ++ idx
        }
      | Branch(arr) => `\n` ++ indent(d) ++ "[ " ++ arr
        ->Array.reduceWithIndex("", (acc, subtree, j) => {
          let curr = subtree->aux(d+1, j)
          let sep = j > 0 ? ", " : ""
          let sep = if acc->Js.String2.endsWith("]") {
              sep ++ (curr->Js.String2.endsWith("]") ? "" : `\n`) ++ indent(d+1)
            } else { sep }
          acc ++ sep ++ curr
        }) ++ ` ]`
      }
    }
    tree->aux(0, 0)
  }
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
