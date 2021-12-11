open Belt
open Helper

let data = Input.read("./src/data/input_day10.txt")

let sample = 
"[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"

let naviData = data->Input.toLines
  ->Array.map(line => line->Js.String2.trim
    ->Js.String2.split("")->ArrayExt.filterEmptyStr->List.fromArray)


// Part 1 *

type context = Empty | Round | Square | Curly | Angled

exception ParseError

let rec parse = (stream, ctx) => switch stream {
  | list{} => (true, list{})

  | list{"(", ...r} => r->parseNext(ctx, Round)
  | list{")", ...r} => if ctx == Round { (true, r) } else { (false, list{")"}) }

  | list{"[", ...r} => r->parseNext(ctx, Square)
  | list{"]", ...r} => if ctx == Square { (true, r) } else { (false, list{"]"}) }

  | list{"{", ...r} => r->parseNext(ctx, Curly)
  | list{"}", ...r} => if ctx == Curly { (true, r) } else { (false, list{"}"}) }

  | list{"<", ...r} => r->parseNext(ctx, Angled)
  | list{">", ...r} => if ctx == Angled { (true, r) } else { (false, list{">"}) }

  | _ => raise(ParseError)
}
and parseNext = (stream, ctx, nextCtx) => {
  let (ok, r) = stream->parse(nextCtx)
  if ok { r->parse(ctx) } else { (false, r) }
}


let corruptionResults = naviData->Array.map(line => line->parse(Empty))

let errorScore = corruptionResults->Array.reduce(0, (sum, (ok, illegalChar)) =>
    if !ok {
      switch illegalChar {
        | list{")"} => sum + 3
        | list{"]"} => sum + 57
        | list{"}"} => sum + 1197
        | list{">"} => sum + 25137
        | _ => raise(Not_found)
      }
    } else { sum }
  )

Js.log2(corruptionResults, errorScore)


// Part 2 *

let getClosingChar = ctx => switch ctx {
  | Empty => "" | Round => ")" | Square => "]" | Curly => "}" | Angled => ">"
}

let rec parse = (stream, ctxs) => switch stream {
  | list{} => ctxs->List.map(ctx => ctx->getClosingChar)

  | list{"(", ...r} => list{"(", ...r->parse(ctxs->List.add(Round))}
  | list{")", ...r} if ctxs->List.headExn == Round =>
      list{")", ...r->parse(ctxs->List.tailExn)}

  | list{"[", ...r} => list{"[", ...r->parse(ctxs->List.add(Square))}
  | list{"]", ...r} if ctxs->List.headExn == Square =>
      list{"]", ...r->parse(ctxs->List.tailExn)}

  | list{"{", ...r} => list{"{", ...r->parse(ctxs->List.add(Curly))}
  | list{"}", ...r} if ctxs->List.headExn == Curly =>
      list{"}", ...r->parse(ctxs->List.tailExn)}

  | list{"<", ...r} => list{"<", ...r->parse(ctxs->List.add(Angled))}
  | list{">", ...r} if ctxs->List.headExn == Angled =>
      list{">", ...r->parse(ctxs->List.tailExn)}

  | _ => raise(ParseError)
}

let naviData_ok = naviData->Array.keepWithIndex((_,i) =>
  switch corruptionResults->Array.get(i) {
    | Some((ok, _)) => ok
    | None => raise(Not_found)
    }
  )

let naviData_corrected = naviData_ok->Array.map(line => line->parse(list{Empty}))

let corrections = naviData_corrected->Array.mapWithIndex((i,line) => line
  ->List.drop( naviData_ok->Array.getExn(i)->List.length )->Option.getExn )


let completionScores = corrections->Array.map(line => line
  ->List.reduce(BigInt.zero, (total, ch) => {
      let score = switch ch {
        | ")" => BigInt.make(1)
        | "]" => BigInt.make(2)
        | "}" => BigInt.make(3)
        | ">" => BigInt.make(4)
        | _ => BigInt.zero
        }
      if BigInt.gr(score,BigInt.zero) {
        BigInt.add( BigInt.mulInt(total, 5), score )
      } else { total }
    })
  )->SortArray.stableSortBy(BigInt.cmp)

let getMiddleScore = scores => scores
  ->Array.getExn((scores->Array.length-1) / 2)

Js.log2(corrections->Array.map(line => line->List.toArray->Js.Array2.joinWith("")),
        completionScores->getMiddleScore)

