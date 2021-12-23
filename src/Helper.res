open Belt

/** Library extension for arrays */
module ArrayExt = {

  /** Discards all empty strings in given array */
  let filterEmptyStr = strArr => strArr
    ->Array.keep(str => str != "")

  /** Parses all strings in given array as ints (unsafe) */
  let toIntArrExn = strArr => strArr
    ->Array.map(nStr => nStr->Int.fromString->Option.getExn)
    
  let flatMap = (arr, fn) => arr->Array.reduce([], (acc, x) =>
    acc->Array.concat(x->fn) )

  /**
   * Cartesian product for Arrays
   */
  let cartesProd = (arr, dim) => {
    let rec aux = (arr, seq, n) => {
      if n > 0 {
        arr->flatMap(x => arr->aux(seq->Array.concat([x]), n-1 ))
      } else {
        [ seq ]
      }
    }
    arr->aux([], dim)
  }
}

/** Library extension for floats */
module FloatExt = {

  /** Asserts if given float has a decimal place */
  let hasDecimal = (x) => Float.fromInt(Float.toInt(x)) < x
}

/** Library extension for strings */
module StringExt = {

  /** If the length of given string is not a multiple of n,
   *  pads the string with "0" or given padStr from left
   *  or from right if toEnd == true
   */
  let padToXn = (~toEnd=false, ~padStr="0", str, n) => {
    let diff = n - mod(str->Js.String2.length, n)
    if diff < n {
      if toEnd { 
        str ++ padStr->Js.String2.repeat(diff)
      } else {
        padStr->Js.String2.repeat(diff) ++ str
      }
    } else { str }
  }
}

/** Helper functions for number systems beyond decimal  */
module NumSys = {

  /** Converts hexadecimal to binary number string */
  let hexToBin = (~pad=false, hexStr) => {
    let n = ("0x" ++ hexStr)->Js.Float.fromString

    if n->Js.Float.isNaN { None } else {
      let binStr = n->Js.Float.toStringWithRadix(~radix=2)
      
      Some(pad ? binStr->StringExt.padToXn(4) : binStr)
    }
  }
    /* ->Option.map(num => num */
    /*   ->Js.Float.toStringWithRadix(~radix=2) */
    /*   ->StringExt.padToXn(4)) */
}

module Input = {

  /** Reads files synchronously using the node.js API
   *  (see https://www.geeksforgeeks.org/node-js-fs-readfilesync-method/)
   */
  let read = path => Node_fs.readFileSync(path, #utf8)

  /** Splits input string into array at line-breaks, discards empty lines */
  let toLines = str => str->Js.String2.split("\n")
    ->ArrayExt.filterEmptyStr

}

/** Helper module for tuples (a,b)
 *  with comparators to use in e.g. Sets or Maps
 */
module Tuple = {

  let has = elem => ( ((a,b)) => a == elem || b == elem )
  let hasNot = elem => ( ((a,b)) => a != elem && b != elem )

  let fst = ((a,_)) => a
  let snd = ((_,b)) => b
  let get = ((a,b), i) => if i <= 0 { a } else { b }

  let reduce = (pair, init, reducerFn) => {

    let aux = (acc, pair, i) => pair->get(i)->reducerFn(acc, pair, i)
    aux(aux(init, pair, 0), pair, 1)
  }

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

  let getArrBounds = (~minFrom=Js.Int.max, ~maxFrom=0, tp) => tp
    ->Array.reduce((minFrom, maxFrom), ((min,max), n) =>
      (n < min ? n : min, n > max ? n : max))

}

/** Generic n-tree data structure */
module Tree = {
  type rec t<'a> = Leaf('a) | Branch(array<t<'a>>)

  /** Prints the tree in a readable indented format
   *  (displays additional info about depth and order of Leafs in <…>)
   */
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

/** Binding module for BigInt type in JS */
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
