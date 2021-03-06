// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Helper from "./Helper.bs.js";
import * as Belt_List from "../node_modules/rescript/lib/es6/belt_List.js";
import * as Belt_Array from "../node_modules/rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "../node_modules/rescript/lib/es6/belt_Option.js";
import * as Belt_SortArray from "../node_modules/rescript/lib/es6/belt_SortArray.js";
import * as Caml_exceptions from "../node_modules/rescript/lib/es6/caml_exceptions.js";

var data = Helper.Input.read("./src/data/input_day10.txt");

var naviData = Belt_Array.map(Helper.Input.toLines(data), (function (line) {
        return Belt_List.fromArray(Helper.ArrayExt.filterEmptyStr(line.trim().split("")));
      }));

var ParseError = /* @__PURE__ */Caml_exceptions.create("Day10.ParseError");

function parse(stream, ctx) {
  if (!stream) {
    return [
            true,
            /* [] */0
          ];
  }
  switch (stream.hd) {
    case "(" :
        return parseNext(stream.tl, ctx, /* Round */1);
    case ")" :
        if (ctx === /* Round */1) {
          return [
                  true,
                  stream.tl
                ];
        } else {
          return [
                  false,
                  {
                    hd: ")",
                    tl: /* [] */0
                  }
                ];
        }
    case "<" :
        return parseNext(stream.tl, ctx, /* Angled */4);
    case ">" :
        if (ctx === /* Angled */4) {
          return [
                  true,
                  stream.tl
                ];
        } else {
          return [
                  false,
                  {
                    hd: ">",
                    tl: /* [] */0
                  }
                ];
        }
    case "[" :
        return parseNext(stream.tl, ctx, /* Square */2);
    case "]" :
        if (ctx === /* Square */2) {
          return [
                  true,
                  stream.tl
                ];
        } else {
          return [
                  false,
                  {
                    hd: "]",
                    tl: /* [] */0
                  }
                ];
        }
    case "{" :
        return parseNext(stream.tl, ctx, /* Curly */3);
    case "}" :
        if (ctx === /* Curly */3) {
          return [
                  true,
                  stream.tl
                ];
        } else {
          return [
                  false,
                  {
                    hd: "}",
                    tl: /* [] */0
                  }
                ];
        }
    default:
      throw {
            RE_EXN_ID: ParseError,
            Error: new Error()
          };
  }
}

function parseNext(stream, ctx, nextCtx) {
  var match = parse(stream, nextCtx);
  var r = match[1];
  if (match[0]) {
    return parse(r, ctx);
  } else {
    return [
            false,
            r
          ];
  }
}

var corruptionResults = Belt_Array.map(naviData, (function (line) {
        return parse(line, /* Empty */0);
      }));

var errorScore = Belt_Array.reduce(corruptionResults, 0, (function (sum, param) {
        if (param[0]) {
          return sum;
        }
        var illegalChar = param[1];
        if (illegalChar) {
          switch (illegalChar.hd) {
            case ")" :
                if (illegalChar.tl) {
                  throw {
                        RE_EXN_ID: "Not_found",
                        Error: new Error()
                      };
                }
                return sum + 3 | 0;
            case ">" :
                if (illegalChar.tl) {
                  throw {
                        RE_EXN_ID: "Not_found",
                        Error: new Error()
                      };
                }
                return sum + 25137 | 0;
            case "]" :
                if (illegalChar.tl) {
                  throw {
                        RE_EXN_ID: "Not_found",
                        Error: new Error()
                      };
                }
                return sum + 57 | 0;
            case "}" :
                if (illegalChar.tl) {
                  throw {
                        RE_EXN_ID: "Not_found",
                        Error: new Error()
                      };
                }
                return sum + 1197 | 0;
            default:
              throw {
                    RE_EXN_ID: "Not_found",
                    Error: new Error()
                  };
          }
        } else {
          throw {
                RE_EXN_ID: "Not_found",
                Error: new Error()
              };
        }
      }));

console.log(corruptionResults, errorScore);

function getClosingChar(ctx) {
  switch (ctx) {
    case /* Empty */0 :
        return "";
    case /* Round */1 :
        return ")";
    case /* Square */2 :
        return "]";
    case /* Curly */3 :
        return "}";
    case /* Angled */4 :
        return ">";
    
  }
}

function parse$1(stream, ctxs) {
  if (!stream) {
    return Belt_List.map(ctxs, getClosingChar);
  }
  switch (stream.hd) {
    case "(" :
        return {
                hd: "(",
                tl: parse$1(stream.tl, Belt_List.add(ctxs, /* Round */1))
              };
    case ")" :
        if (Belt_List.headExn(ctxs) === /* Round */1) {
          return {
                  hd: ")",
                  tl: parse$1(stream.tl, Belt_List.tailExn(ctxs))
                };
        }
        throw {
              RE_EXN_ID: ParseError,
              Error: new Error()
            };
    case "<" :
        return {
                hd: "<",
                tl: parse$1(stream.tl, Belt_List.add(ctxs, /* Angled */4))
              };
    case ">" :
        if (Belt_List.headExn(ctxs) === /* Angled */4) {
          return {
                  hd: ">",
                  tl: parse$1(stream.tl, Belt_List.tailExn(ctxs))
                };
        }
        throw {
              RE_EXN_ID: ParseError,
              Error: new Error()
            };
    case "[" :
        return {
                hd: "[",
                tl: parse$1(stream.tl, Belt_List.add(ctxs, /* Square */2))
              };
    case "]" :
        if (Belt_List.headExn(ctxs) === /* Square */2) {
          return {
                  hd: "]",
                  tl: parse$1(stream.tl, Belt_List.tailExn(ctxs))
                };
        }
        throw {
              RE_EXN_ID: ParseError,
              Error: new Error()
            };
    case "{" :
        return {
                hd: "{",
                tl: parse$1(stream.tl, Belt_List.add(ctxs, /* Curly */3))
              };
    case "}" :
        if (Belt_List.headExn(ctxs) === /* Curly */3) {
          return {
                  hd: "}",
                  tl: parse$1(stream.tl, Belt_List.tailExn(ctxs))
                };
        }
        throw {
              RE_EXN_ID: ParseError,
              Error: new Error()
            };
    default:
      throw {
            RE_EXN_ID: ParseError,
            Error: new Error()
          };
  }
}

var naviData_ok = Belt_Array.keepWithIndex(naviData, (function (param, i) {
        var match = Belt_Array.get(corruptionResults, i);
        if (match !== undefined) {
          return match[0];
        }
        throw {
              RE_EXN_ID: "Not_found",
              Error: new Error()
            };
      }));

var naviData_corrected = Belt_Array.map(naviData_ok, (function (line) {
        return parse$1(line, {
                    hd: /* Empty */0,
                    tl: /* [] */0
                  });
      }));

var corrections = Belt_Array.mapWithIndex(naviData_corrected, (function (i, line) {
        return Belt_Option.getExn(Belt_List.drop(line, Belt_List.length(Belt_Array.getExn(naviData_ok, i))));
      }));

var completionScores = Belt_SortArray.stableSortBy(Belt_Array.map(corrections, (function (line) {
            return Belt_List.reduce(line, Helper.$$BigInt.zero, (function (total, ch) {
                          var score;
                          switch (ch) {
                            case ")" :
                                score = BigInt(1);
                                break;
                            case ">" :
                                score = BigInt(4);
                                break;
                            case "]" :
                                score = BigInt(2);
                                break;
                            case "}" :
                                score = BigInt(3);
                                break;
                            default:
                              score = Helper.$$BigInt.zero;
                          }
                          if (Helper.$$BigInt.gr(score, Helper.$$BigInt.zero)) {
                            return Helper.$$BigInt.add(Helper.$$BigInt.mulInt(total, 5), score);
                          } else {
                            return total;
                          }
                        }));
          })), Helper.$$BigInt.cmp);

function getMiddleScore(scores) {
  return Belt_Array.getExn(scores, (scores.length - 1 | 0) / 2 | 0);
}

console.log(Belt_Array.map(corrections, (function (line) {
            return Belt_List.toArray(line).join("");
          })), getMiddleScore(completionScores));

var sample = "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]";

export {
  data ,
  sample ,
  naviData ,
  ParseError ,
  parseNext ,
  corruptionResults ,
  errorScore ,
  getClosingChar ,
  parse$1 as parse,
  naviData_ok ,
  naviData_corrected ,
  corrections ,
  completionScores ,
  getMiddleScore ,
  
}
/* data Not a pure module */
