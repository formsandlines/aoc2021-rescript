open Belt
open Helper

module Digit = {
  type unary = [ #0 ]
  type binary = [ unary | #1 ]
  type ternary = [ binary | #2 ]
  type quaternary = [ ternary | #3 ]
  type quinary = [ quaternary | #4 ]
  type senary = [ quinary | #5 ]
  type septenary = [ senary | #6 ]
  type octal = [ septenary | #7 ]
  type nonary = [ octal | #8 ]

  type decimal = [ nonary | #9 ]
  type undecimal = [ decimal | #a ]
  type duodecimal = [ undecimal | #b ]
  type tridecimal = [ duodecimal | #c ]
  type tetradecimal = [ tridecimal | #d ]
  type pentadecimal = [ tetradecimal | #e ]
  type hexadecimal = [ pentadecimal | #f ]

  type t = hexadecimal

  module CmpDigit =
    Belt.Id.MakeComparable({
      type t = t
      let cmp = (a, b) => Pervasives.compare(a, b)
      })

  let unarySet = Set.fromArray([ #0 ], ~id=module(CmpDigit))
  let binarySet = unarySet->Set.add( #1 )
  let ternarySet = binarySet->Set.add( #2 )
  let quaternarySet = ternarySet->Set.add( #3 )
  let quinarySet = quaternarySet->Set.add( #4 )
  let senarySet = quinarySet->Set.add( #5 )
  let septenarySet = senarySet->Set.add( #6 )
  let octalSet = septenarySet->Set.add( #7 )
  let nonarySet = octalSet->Set.add( #8 )
  let decimalSet = nonarySet->Set.add( #9 )
  let undecimalSet = decimalSet->Set.add( #a )
  let duodecimalSet = undecimalSet->Set.add( #b )
  let tridecimalSet = duodecimalSet->Set.add( #c )
  let tetradecimalSet = tridecimalSet->Set.add( #d )
  let pentadecimalSet = tetradecimalSet->Set.add( #e )
  let hexadecimalSet = pentadecimalSet->Set.add( #f )

  let pred = digit => switch digit {
    | #0 => None     | #1 => Some(#0) | #2 => Some(#1) | #3 => Some(#2)
    | #4 => Some(#3) | #5 => Some(#4) | #6 => Some(#5) | #7 => Some(#6)
    | #8 => Some(#7) | #9 => Some(#8) | #a => Some(#9) | #b => Some(#a)
    | #c => Some(#b) | #d => Some(#c) | #e => Some(#d) | #f => Some(#e)
    }

  let succ = digit => switch digit {
    | #0 => Some(#1) | #1 => Some(#2) | #2 => Some(#3) | #3 => Some(#4)
    | #4 => Some(#5) | #5 => Some(#6) | #6 => Some(#7) | #7 => Some(#8)
    | #8 => Some(#9) | #9 => Some(#a) | #a => Some(#b) | #b => Some(#c)
    | #c => Some(#d) | #d => Some(#e) | #e => Some(#f) | #f => None
    }

  let show = digit => switch digit {
    | #0 => "0" | #1 => "1" | #2 => "2" | #3 => "3"
    | #4 => "4" | #5 => "5" | #6 => "6" | #7 => "7"
    | #8 => "8" | #9 => "9" | #a => "a" | #b => "b"
    | #c => "c" | #d => "d" | #e => "e" | #f => "f"
  }

  let fromStr = d_str => switch d_str->Js.String2.toLowerCase {
    | "0" => Some(#0) | "1" => Some(#1) | "2" => Some(#2) | "3" => Some(#3)
    | "4" => Some(#4) | "5" => Some(#5) | "6" => Some(#6) | "7" => Some(#7)
    | "8" => Some(#8) | "9" => Some(#9) | "a" => Some(#a) | "b" => Some(#b)
    | "c" => Some(#c) | "d" => Some(#d) | "e" => Some(#e) | "f" => Some(#f)
    | _ => None
    }
  let fromStrExn = d_str => d_str->fromStr->Option.getUnsafe

  let allBases = digit => switch digit {
    | #...unary => [1]
    | #...binary => Array.range(1,2)
    | #...ternary => Array.range(1,3)
    | #...quaternary => Array.range(1,4)
    | #...quinary => Array.range(1,5)
    | #...senary => Array.range(1,6)
    | #...septenary => Array.range(1,7)
    | #...octal => Array.range(1,8)
    | #...nonary => Array.range(1,9)
    | #...decimal => Array.range(1,10)
    | #...undecimal => Array.range(1,11)
    | #...duodecimal => Array.range(1,12)
    | #...tridecimal => Array.range(1,13)
    | #...tetradecimal => Array.range(1,14)
    | #...pentadecimal => Array.range(1,15)
    | #...hexadecimal => Array.range(1,16)
    }

  let hasBase = (digit, base) => switch base {
    | 0 | 1 => true // always true
    | 2 => binarySet->Set.has(digit)
    | 3 => ternarySet->Set.has(digit)
    | 4 => quaternarySet->Set.has(digit)
    | 5 => quinarySet->Set.has(digit)
    | 6 => senarySet->Set.has(digit)
    | 7 => septenarySet->Set.has(digit)
    | 8 => octalSet->Set.has(digit)
    | 9 => nonarySet->Set.has(digit)
    | 10 => decimalSet->Set.has(digit)
    | 11 => undecimalSet->Set.has(digit)
    | 12 => duodecimalSet->Set.has(digit)
    | 13 => tridecimalSet->Set.has(digit)
    | 14 => tetradecimalSet->Set.has(digit)
    | 15 => pentadecimalSet->Set.has(digit)
    | 16 => hexadecimalSet->Set.has(digit)
    | _ => false // undefined
    }

  /* let makeBinary: [> hexadecimal] => option<binary> */
  /*   = d => d->isBase(2) ? Some(d) : None */
  let makeBinary: [> t] => option<binary>
    = digit => switch digit {
      | #0 => Some(#0) | #1 => Some(#1)
      | _ => None
      }

  let makeDecimal: [> t] => option<decimal>
    = digit => switch digit {
      | #0 => Some(#0) | #1 => Some(#1) | #2 => Some(#2) | #3 => Some(#3)
      | #4 => Some(#4) | #5 => Some(#5) | #6 => Some(#6) | #7 => Some(#7)
      | #8 => Some(#8) | #9 => Some(#9)
      | _ => None
      }

  let makeHexadecimal: [> t] => option<hexadecimal>
    = digit => Some(digit)

  let makeBaseListExn: type base. (list<[> t]>, [> t] => option<base>) => list<base>
    = (d_lst, fn_toBase) =>
      d_lst->List.map(d => d->fn_toBase->Option.getExn)

  let makeBaseList: type base. (list<[> t]>, [> t] => option<base>) => option<list<base>>
    = (d_lst, fn_toBase) => {
      let base_lst = d_lst->List.keepMap(d => d->fn_toBase)
      if base_lst->List.length == d_lst->List.length { Some(base_lst) } else { None }
    }

  let makeBaseArrExn: type base. (array<[> t]>, [> t] => option<base>) => array<base>
    = (d_arr, fn_toBase) =>
      d_arr->Array.map(d => d->fn_toBase->Option.getExn)

  let makeBaseArr: type base. (array<[> t]>, [> t] => option<base>) => option<array<base>>
    = (d_arr, fn_toBase) => {
      let base_arr = d_arr->Array.keepMap(d => d->fn_toBase)
      if base_arr->Array.length == d_arr->Array.length { Some(base_arr) } else { None }
    }

}


module Number = {

  type binary = [ #Binary(list<Digit.binary>) ]
  type decimal = [ #Decimal(list<Digit.decimal>) ]
  type hexadecimal = [ #Hexadecimal(list<Digit.hexadecimal>) ]
  type t = [ binary | decimal | hexadecimal ]

  let show = num => {
    let showDigits = num => num->List.reduce("", (str,d) => str ++ d->Digit.show)
    switch num {
      | #Binary(ds) => "base 2: " ++ ds->showDigits
      | #Decimal(ds) => "base 10: " ++ ds->showDigits
      | #Hexadecimal(ds) => "base 16: " ++ ds->showDigits
      }
  }

  let make = (d_lst, base) => {
    switch base {
      | 2 => d_lst->Digit.makeBaseList(Digit.makeBinary)
          ->Option.map(num => #Binary(num))
      | 10 => d_lst->Digit.makeBaseList(Digit.makeDecimal)
          ->Option.map(num => #Decimal(num))
      | 16 => d_lst->Digit.makeBaseList(Digit.makeHexadecimal)
          ->Option.map(num => #Hexadecimal(num))
      | _ => None
      }
  }

  let fromStrWithRadix = (~radix, n_str) => {
    let str_arr = n_str->Js.String2.split("")
    let d_arr = str_arr->Array.keepMap(d_str => d_str->Digit.fromStr)

    if d_arr->Array.length == str_arr->Array.length {
      d_arr->List.fromArray->make(radix)
    } else { None }
  }

  /* let fromInt = (~radix, n) => { */
  /*   let dec = n->Int.toString->fromStrWithRadix(~radix=10) */
  /* } */

  /* let getDigitArr = num => switch num { */
  /*   | #Binary(ds) | #Decimal(ds) | #Hexadecimal(ds) => ds */
  /*   } */


  let pred = num => switch num {
    | #Binary(ds) => {
        let max_d = Digit.binarySet->Set.maximum->Option.getExn

        let rec aux = (ds, ds_pred) => switch ds {
          | list{} => list{}
          | list{#0, ...r} => r->aux( ds_pred->List.add(max_d) )
          | list{d, ...r} =>
              switch d->Digit.pred {
                | Some(d_) => r->List.reverseConcat(ds_pred->List.add(d_))
                | None => raise(Not_found)
              }
          }

        Some( ds->List.reverse->aux(list{}) )
      }
    | _ => None
    }


  /* let convert = (d_arr, digitSet) => { */
  /*   let max = digitSet->Set.maximum */

  /*   let rec aux = (d_arr) => { */

  /*     let pred = */ 

  /*   } */

  /* } */

  /* let toBinary = num => switch num { */
  /*   | #Binary(_) => num */
  /*   | #Decimal(ds) => */ 
  /*   } */

  /* type rec t<'base> = */
  /*   | Unary(array<Digit.unary>): t<Digit.unary> */
  /*   | Binary(array<Digit.binary>): t<Digit.binary> */
  /*   | Decimal(array<Digit.decimal>): t<Digit.decimal> */
  /*   | Hexadecimal(array<Digit.hexadecimal>): t<Digit.hexadecimal> */

  /* let makeBinaryExn = d_arr => Binary(d_arr->Digit.makeBaseArrExn(Digit.makeBinary)) */
  /* let makeBinary = d_arr => d_arr->Digit.makeBaseArr(Digit.makeBinary) */
  /*   ->Option.map(d_bin => Binary(d_bin)) */

  /* let makeDecimalExn = d_arr => Decimal(d_arr->Digit.makeBaseArrExn(Digit.makeDecimal)) */
  /* let makeDecimal = d_arr => d_arr->Digit.makeBaseArr(Digit.makeDecimal) */
  /*   ->Option.map(d_bin => Decimal(d_bin)) */

  /* let makeExn: type base. (array<[> Digit.t]>, [> Digit.t] => option<base>) => array<base> */
  /*   = (d_arr, fn_toBase) => */
  /*     d_arr->Array.map(d => d->fn_toBase->Option.getExn) */


  /* let make: type base. (array<base>, int) => option<t<base>> */
  /*   = (d_arr, base) => { */

  /*   let maybeConverter: type base. (array<base>, int) => option< */
  /*       ( [> Digit.t] => option<base>, */
  /*         array<Digit.binary> => t<base> ) */
  /*     > = (_, base) => switch base { */
  /*     | 2 => Some(Digit.makeBinary, num => Binary(num)) */
  /*     | 10 => Some(Digit.makeDecimal, num => Decimal(num)) */
  /*     | _ => None */
  /*     } */
  /*   d_arr->maybeConverter(base)->Option.flatMap((fn_toBase, fn_toNum) => { */
  /*     d_arr->Digit.makeBaseArr(fn_toBase) */
  /*       ->Option.map(fn_toNum) */
  /*   }) */
  /* } */

  /* let fromStrWithRadix = (~radix, n_str) => { */
  /*   let str_arr = n_str->Js.String2.split("") */
  /*   let d_arr = str_arr->Array.keepMap(d_str => d_str->Digit.fromStr) */

  /*   if d_arr->Array.length == str_arr->Array.length { */
  /*     d_arr->make(radix) */
  /*   } else { None } */
  /* } */
  
  /* type unary = list<Digit.unary> */

  /* let fromStrNumb = n_str => n_str->Js.String2.split("") */
  /*   ->String.map(d_str => d_str->fromStrDigit) */
}
