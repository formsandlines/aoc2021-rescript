open Belt
open Helper


let processInput = input => {
  input->NumSys.hexToBin(~pad=true)->Option.getExn
    ->Js.String2.split("")
    /* ->ArrayExt.toIntArrExn */
    ->List.fromArray
}

module Packet = {
  type rec t = Lit(float) | Op(list<t>)

  exception ParseError(string)

  let parseLiteral = binLst => {

    let rec aux = (binLst, acc) => switch binLst {
      | list{"0",b4,b3,b2,b1, ...r} => acc ++ b4 ++ b3 ++ b2 ++ b1
      | list{"1",b4,b3,b2,b1, ...r} => r->aux(acc ++ b4 ++ b3 ++ b2 ++ b1)
      | _ => raise(ParseError("Literal not well-formed"))
      }
    switch binLst->aux("")->FloatExt.fromStrWithRadix(~radix=2.) {
      | Some(n) => n
      | None => raise(ParseError("Literal not in binary format!"))
      }
  }

  let parseBinAsInt = (binLst, n) =>
    binLst->List.splitAt(n)->Option.flatMap(((bitLen, r)) =>
      switch bitLen->List.reduce("", (bin,bit) => bin ++ bit)
        ->FloatExt.fromStrWithRadix(~radix=2.) {
        | Some(len) => Some((len, r))
        | None => None
        })

  let rec parseOperator = binLst => {
    switch binLst { // lenTypeID
      | list{"0", ...r} => {
          let (len, r') = r->parseBinAsInt(15)->Option.getWithDefault(
              raise(ParseError("Bad length of sub-packets!")))

          switch r'->List.splitAt(len->Int.fromFloat) {
            | Some((litLst, r'')) => litLst->parseHeader
            | None => raise(ParseError("Corrupt sub-packets!"))
          }
        }
      | list{"1", ...r} => {
          let (count, r') = r->parseBinAsInt(11)->Option.getWithDefault(
              raise(ParseError("Bad count of sub-packets!")))

          // how to determine sub-packets from count?
        }
      | _ => raise(ParseError("Bad length type ID in operator!"))
      }
  }
  and parseHeader = binLst => switch binLst {
    | list{v3,v2,v1, "1","0","0", ...r} => Lit(r->parseLiteral)
    | list{v3,v2,v1, t3,t2,t1, ...r} => Op(r->parseOperator)
    | _ => raise(ParseError("Header not well-formed"))
    }

}

let solve1 = input => {
  Js.log(input)
}


let data = Input.read("./src/data/input_day16.txt")

let ex1 =
"38006F45291200"

let input = ex1->processInput

solve1(input)

/*
---------------
PACKET ENCODING

Packet header:
 - 3 bits: packet version
 - 3 bits: packet type ID:
   - type 4 (100) => literal value
   - everything not type 4 => operator packet

Literal values:
 - ...Header
 - broken into groups of 4 bits + 1 prefix-bit
 - prefix-bit: 1 each group and 0 for last

Operator packets:
 - ...Header
 - 1 bit: length type ID:
   - 0: next 15 bits => total bit-length of sub-packets
   - 1: next 11 bits => number of sub-packets

-------
EXAMPLE

Hex string:
EE00D40C823060

In binary:
11101110000000001101010000001100100000100011000001100000

Parsing:
  111 011 1 00000000011
  v:7 t:3 lenType:1   subPackets:3
    010 100 0 0001
    v:2 t:4 lastGroup:1
    100 100 0 0010
    v:4 t:4 lastGroup:2
    001 100 0 0011
    v:1 t:4 lastGroup:3
    00000
    meaningless zeros until lenght is multiple of 4 bits
*/
