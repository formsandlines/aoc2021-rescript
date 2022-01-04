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

module SetExt = {

  module CmpStr =
    Belt.Id.MakeComparable({
      type t = Set.String.t
      let cmp = Set.String.cmp
    })
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

/** Simple n-ary tree structure based on arrays */
module ArrayTree = {
  type rec t<'a> = Branch('a, array<t<'a>>)

  /** Prints the tree in a readable depth-indented format:
   *  "<depth>-<order>: <node>"
   */
  let show = (tree, showFn) => {
    let indent = n => `  `->Js.String2.repeat(n)

    let rec aux = (Branch(node, subtrees), d, i) => {
      let idx = `${d->Int.toString}-${i->Int.toString}`
      `\n${indent(d) ++ idx}: ${node->showFn}` ++ subtrees
        ->Array.reduceWithIndex("", (acc, subtree, j) =>
          acc ++ subtree->aux(d+1, j))
    }
    tree->aux(0, 0)
  }

  /** Finds all paths from a given end node down to the root
   *  returns an array of index-lists (index of deepest node, …, root == 0)
   */
  let findPaths = (tree, endNode) => {

    let rec aux = (tree: t<'a>, index) => switch tree {
      | Branch(node, []) => if node == endNode { [index] } else { [] }
      | Branch(_, subtrees) => subtrees
        ->Array.reduceWithIndex([], (acc, subtree, i) => acc->Array.concat(
          subtree->aux( index->List.add(i) )) )
      }
    tree->aux(list{0})
  }

  /** Gets all nodes from an index list (e.g. from findPaths) */
  let getPathFromIndex = (indexList, tree) => {

    let rec aux = (indexList, tree: t<string>) => switch tree {
      | Branch(node, []) => list{ node }
      | Branch(node, subtrees) => switch indexList {
          | list{} => list{}
          | list{i, ...rest} => switch subtrees->Array.get(i) {
            | Some(subtree) => list{ node, ...rest->aux(subtree) }
            | None => raise(Not_found)
          }
        }
      }
    indexList->List.tailExn->aux(tree)
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
