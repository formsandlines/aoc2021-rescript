// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "../node_modules/rescript/lib/es6/curry.js";
import * as Helper from "./Helper.bs.js";
import * as Js_math from "../node_modules/rescript/lib/es6/js_math.js";
import * as Belt_Int from "../node_modules/rescript/lib/es6/belt_Int.js";
import * as Caml_obj from "../node_modules/rescript/lib/es6/caml_obj.js";
import * as Belt_List from "../node_modules/rescript/lib/es6/belt_List.js";
import * as Belt_Array from "../node_modules/rescript/lib/es6/belt_Array.js";
import * as Caml_option from "../node_modules/rescript/lib/es6/caml_option.js";
import * as Caml_exceptions from "../node_modules/rescript/lib/es6/caml_exceptions.js";

function show(sfn) {
  if (sfn.TAG === /* Num */0) {
    return String(sfn._0);
  } else {
    return "[" + show(sfn._0) + "," + show(sfn._1) + "]";
  }
}

var ParseError = /* @__PURE__ */Caml_exceptions.create("Day18.SFNum.Syntax.ParseError");

function isDigit(ch) {
  switch (ch) {
    case "0" :
    case "1" :
    case "2" :
    case "3" :
    case "4" :
    case "5" :
    case "6" :
    case "7" :
    case "8" :
    case "9" :
        return true;
    default:
      return false;
  }
}

function parseExpr(_stream, _expr) {
  while(true) {
    var expr = _expr;
    var stream = _stream;
    if (stream) {
      var d = stream.hd;
      switch (d) {
        case "," :
            _stream = stream.tl;
            continue ;
        case "[" :
            var match = parseExpr(stream.tl, []);
            _expr = Belt_Array.concat(expr, [match[0]]);
            _stream = match[1];
            continue ;
        case "]" :
            if (expr.length !== 2) {
              throw {
                    RE_EXN_ID: ParseError,
                    _1: "Bad pair syntax",
                    Error: new Error()
                  };
            }
            var a = expr[0];
            var b = expr[1];
            return [
                    {
                      TAG: /* Pair */1,
                      _0: a,
                      _1: b
                    },
                    stream.tl
                  ];
        default:
          if (isDigit(d)) {
            var match$1 = parseNum(stream.tl, d);
            _expr = Belt_Array.concat(expr, [match$1[0]]);
            _stream = match$1[1];
            continue ;
          }
          throw {
                RE_EXN_ID: ParseError,
                _1: "Expected ']'",
                Error: new Error()
              };
      }
    } else {
      throw {
            RE_EXN_ID: ParseError,
            _1: "Expected ']'",
            Error: new Error()
          };
    }
  };
}

function parseNum(_stream, _nStr) {
  while(true) {
    var nStr = _nStr;
    var stream = _stream;
    if (stream) {
      var d = stream.hd;
      if (isDigit(d)) {
        _nStr = nStr + d;
        _stream = stream.tl;
        continue ;
      }
      
    }
    var n = Belt_Int.fromString(nStr);
    if (n !== undefined) {
      return [
              {
                TAG: /* Num */0,
                _0: n
              },
              stream
            ];
    }
    throw {
          RE_EXN_ID: ParseError,
          _1: "Bad number syntax",
          Error: new Error()
        };
  };
}

function parse(stream) {
  var match = parseExpr(Belt_List.tailExn(stream), []);
  if (match[1]) {
    throw {
          RE_EXN_ID: ParseError,
          _1: "Parsing failed",
          Error: new Error()
        };
  }
  return match[0];
}

var Syntax = {
  ParseError: ParseError,
  isDigit: isDigit,
  parseExpr: parseExpr,
  parseNum: parseNum,
  parse: parse
};

function read(str) {
  return parse(Belt_List.fromArray(str.split("")));
}

function findLeftmostNumPair(minDepthOpt, sfn) {
  var minDepth = minDepthOpt !== undefined ? minDepthOpt : 0;
  var aux = function (_sfn, _idx) {
    while(true) {
      var idx = _idx;
      var sfn = _sfn;
      if (sfn.TAG === /* Num */0) {
        return ;
      }
      var n = sfn._0;
      if (n.TAG === /* Num */0) {
        var m = sfn._1;
        if (m.TAG === /* Num */0) {
          if (Belt_List.length(idx) >= minDepth) {
            return {
                    idx: idx,
                    pair: [
                      n._0,
                      m._0
                    ]
                  };
          } else {
            return ;
          }
        }
        
      }
      var result = aux(n, Belt_List.add(idx, 0));
      if (result !== undefined) {
        return Caml_option.some(Caml_option.valFromOption(result));
      }
      _idx = Belt_List.add(idx, 1);
      _sfn = sfn._1;
      continue ;
    };
  };
  return aux(sfn, /* [] */0);
}

function findAdjacentNum(findLeft, sfn, compIdx) {
  var fn = function (_idx) {
    while(true) {
      var idx = _idx;
      if (!idx) {
        return /* [] */0;
      }
      var match = idx.hd;
      if (match !== 0) {
        if (match !== 1) {
          _idx = idx.tl;
          continue ;
        }
        if (findLeft) {
          return {
                  hd: 0,
                  tl: idx.tl
                };
        }
        _idx = idx.tl;
        continue ;
      }
      if (!findLeft) {
        return {
                hd: 1,
                tl: idx.tl
              };
      }
      _idx = idx.tl;
      continue ;
    };
  };
  var adjPath = fn(compIdx);
  if (Belt_List.length(adjPath) === 0) {
    return ;
  } else {
    var _sfn = sfn;
    var _idx = /* [] */0;
    var _adjPath = Belt_List.reverse(adjPath);
    while(true) {
      var adjPath$1 = _adjPath;
      var idx = _idx;
      var sfn$1 = _sfn;
      if (sfn$1.TAG === /* Num */0) {
        return {
                idx: idx,
                num: sfn$1._0
              };
      }
      var b = sfn$1._1;
      var a = sfn$1._0;
      if (adjPath$1) {
        if (adjPath$1.hd !== 0) {
          _adjPath = adjPath$1.tl;
          _idx = Belt_List.add(idx, 1);
          _sfn = b;
          continue ;
        }
        _adjPath = adjPath$1.tl;
        _idx = Belt_List.add(idx, 0);
        _sfn = a;
        continue ;
      }
      if (findLeft) {
        _adjPath = /* [] */0;
        _idx = Belt_List.add(idx, 1);
        _sfn = b;
        continue ;
      }
      _adjPath = /* [] */0;
      _idx = Belt_List.add(idx, 0);
      _sfn = a;
      continue ;
    };
  }
}

function map(sfn, fn) {
  var aux = function (sfn, idx) {
    if (sfn.TAG === /* Num */0) {
      return {
              TAG: /* Num */0,
              _0: Curry._2(fn, idx, sfn._0)
            };
    } else {
      return {
              TAG: /* Pair */1,
              _0: aux(sfn._0, Belt_List.add(idx, 0)),
              _1: aux(sfn._1, Belt_List.add(idx, 1))
            };
    }
  };
  return aux(sfn, /* [] */0);
}

function flatMap(sfn, fn) {
  var aux = function (sfn, idx) {
    if (sfn.TAG === /* Num */0) {
      return Curry._2(fn, idx, {
                  TAG: /* Num */0,
                  _0: sfn._0
                });
    } else {
      return Curry._2(fn, idx, {
                  TAG: /* Pair */1,
                  _0: aux(sfn._0, Belt_List.add(idx, 0)),
                  _1: aux(sfn._1, Belt_List.add(idx, 1))
                });
    }
  };
  return aux(sfn, /* [] */0);
}

function reduce(sfn, init, reducerFn) {
  var aux = function (acc, sfn, idx) {
    var acc$1 = Curry._3(reducerFn, acc, sfn, idx);
    if (sfn.TAG === /* Num */0) {
      return acc$1;
    } else {
      var param = [
        sfn._0,
        sfn._1
      ];
      var acc$2 = Curry._3(aux, acc$1, param[0], Belt_List.add(idx, 0));
      return Curry._3(aux, acc$2, param[1], Belt_List.add(idx, 1));
    }
  };
  return aux(init, sfn, /* [] */0);
}

function explode(sfn) {
  var toExpl = findLeftmostNumPair(4, sfn);
  if (toExpl === undefined) {
    return [
            sfn,
            false
          ];
  }
  var toExpl$1 = Caml_option.valFromOption(toExpl);
  var leftAdj = findAdjacentNum(true, sfn, toExpl$1.idx);
  var rightAdj = findAdjacentNum(false, sfn, toExpl$1.idx);
  var match = toExpl$1.pair;
  var rightVal = match[1];
  var leftVal = match[0];
  var sfn$1 = flatMap(sfn, (function (idx, sfn) {
          if (sfn.TAG !== /* Num */0) {
            if (Caml_obj.caml_equal(idx, toExpl$1.idx)) {
              return {
                      TAG: /* Num */0,
                      _0: 0
                    };
            } else {
              return sfn;
            }
          }
          var n = sfn._0;
          var tmp;
          var exit = 0;
          if (leftAdj !== undefined && Caml_obj.caml_equal(Caml_option.valFromOption(leftAdj).idx, idx)) {
            tmp = n + leftVal | 0;
          } else {
            exit = 1;
          }
          if (exit === 1) {
            tmp = rightAdj !== undefined && Caml_obj.caml_equal(Caml_option.valFromOption(rightAdj).idx, idx) ? n + rightVal | 0 : n;
          }
          return {
                  TAG: /* Num */0,
                  _0: tmp
                };
        }));
  return [
          sfn$1,
          true
        ];
}

function split(sfn) {
  var aux = function (sfn, hasSplit) {
    if (sfn.TAG === /* Num */0) {
      var n = sfn._0;
      if (!(!hasSplit && n >= 10)) {
        return [
                {
                  TAG: /* Num */0,
                  _0: n
                },
                hasSplit
              ];
      }
      var n_half = n / 2.0;
      var a = {
        TAG: /* Num */0,
        _0: Js_math.floor_int(n_half)
      };
      var b = {
        TAG: /* Num */0,
        _0: Js_math.ceil_int(n_half)
      };
      return [
              {
                TAG: /* Pair */1,
                _0: a,
                _1: b
              },
              true
            ];
    }
    var b$1 = sfn._1;
    var a$1 = sfn._0;
    if (hasSplit) {
      return [
              {
                TAG: /* Pair */1,
                _0: a$1,
                _1: b$1
              },
              hasSplit
            ];
    }
    var match = aux(a$1, hasSplit);
    var hasSplit$1 = match[1];
    var match$1 = hasSplit$1 ? [
        b$1,
        hasSplit$1
      ] : aux(b$1, hasSplit$1);
    return [
            {
              TAG: /* Pair */1,
              _0: match[0],
              _1: match$1[0]
            },
            match$1[1]
          ];
  };
  return aux(sfn, false);
}

function add(sfn_a, sfn_b) {
  var _sfn = {
    TAG: /* Pair */1,
    _0: sfn_a,
    _1: sfn_b
  };
  while(true) {
    var sfn = _sfn;
    var match = explode(sfn);
    var hasExpl = match[1];
    var sfn$1 = match[0];
    var match$1;
    if (hasExpl) {
      match$1 = [
        sfn$1,
        false
      ];
    } else {
      var match$2 = split(sfn$1);
      match$1 = [
        match$2[0],
        match$2[1]
      ];
    }
    var sfn$2 = match$1[0];
    if (!(hasExpl || match$1[1])) {
      return sfn$2;
    }
    _sfn = sfn$2;
    continue ;
  };
}

function magnitude(sfn) {
  if (sfn.TAG === /* Num */0) {
    return sfn._0;
  } else {
    return Math.imul(magnitude(sfn._0), 3) + (magnitude(sfn._1) << 1) | 0;
  }
}

function testFlatmap(param) {
  var label = "Flatmap test 1";
  var param$1 = [
    "[[1,2],[[3,4],5]]",
    {
      hd: 0,
      tl: {
        hd: 1,
        tl: /* [] */0
      }
    }
  ];
  var expected = "[[1,2],[0,5]]";
  var mapIdx = param$1[1];
  var input = param$1[0];
  console.log(label, input);
  var result = flatMap(read(input), (function (idx, sfn) {
          if (Caml_obj.caml_equal(idx, mapIdx)) {
            return {
                    TAG: /* Num */0,
                    _0: 0
                  };
          } else {
            return sfn;
          }
        }));
  console.log("Correct:", show(result) === expected);
  
}

function testReduce(param) {
  var label = "Reduce test 1";
  var input = "[[1,2],[[3,4],5]]";
  var expected = 15;
  console.log(label, input);
  var result = reduce(read(input), 0, (function (acc, sfn, param) {
          var tmp;
          tmp = sfn.TAG === /* Num */0 ? sfn._0 : 0;
          return acc + tmp | 0;
        }));
  console.log("Correct:", result === expected);
  
}

function testExplode(param) {
  var test = function (label, input, expected) {
    console.log(label, input);
    var match = explode(read(input));
    console.log("Exploded:", match[1]);
    var result = show(match[0]);
    console.log("Correct:", result === expected);
    
  };
  test("Explode test 1", "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]", "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]");
  test("Explode test 2", "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]", "[[[[0,7],4],[15,[0,13]]],[1,1]]");
  test("Explode test 3", "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]", "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]");
  test("Explode test 4", "[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]");
  test("Explode test 5", "[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]");
  test("Explode test 6", "[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]");
  test("Explode test 7", "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]");
  return test("Explode test 8", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]");
}

function testSplit(param) {
  var test = function (label, input, expected) {
    console.log(label, input);
    var match = split(read(input));
    console.log("Splitted:", match[1]);
    var result = show(match[0]);
    console.log("Correct:", result === expected);
    
  };
  test("Split test 1", "[[[[0,7],4],[15,[0,13]]],[1,1]]", "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]");
  return test("Split test 2", "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]", "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]");
}

function testAdd(param) {
  var test = function (label, param, expected) {
    console.log(label);
    var sfn_a = read(param[0]);
    var sfn_b = read(param[1]);
    var result = show(add(sfn_a, sfn_b));
    console.log("Expected:", expected);
    console.log("Result:", result);
    console.log("Correct:", result === expected);
    
  };
  test("Add test 1", [
        "[[[[4,3],4],4],[7,[[8,4],9]]]",
        "[1,1]"
      ], "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]");
  test("Add test 2", [
        "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
        "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
      ], "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]");
  test("Add test 3", [
        "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]",
        "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
      ], "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]");
  test("Add test 4", [
        "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]",
        "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
      ], "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]");
  test("Add test 5", [
        "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]",
        "[7,[5,[[3,8],[1,4]]]]"
      ], "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]");
  test("Add test 6", [
        "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]",
        "[[2,[2,2]],[8,[8,1]]]"
      ], "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]");
  return test("Add test 7", [
              "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]",
              "[2,9]"
            ], "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]");
}

var Test = {
  testFlatmap: testFlatmap,
  testReduce: testReduce,
  testExplode: testExplode,
  testSplit: testSplit,
  testAdd: testAdd
};

var SFNum = {
  show: show,
  Syntax: Syntax,
  read: read,
  findLeftmostNumPair: findLeftmostNumPair,
  findAdjacentNum: findAdjacentNum,
  map: map,
  flatMap: flatMap,
  reduce: reduce,
  explode: explode,
  split: split,
  add: add,
  magnitude: magnitude,
  Test: Test
};

function solve1(input) {
  var input$1 = Belt_List.fromArray(input);
  var sum = Belt_List.reduce(Belt_List.tailExn(input$1), Belt_List.headExn(input$1), add);
  console.log(show(sum), magnitude(sum));
  
}

function solve2(input) {
  var candidates = Helper.ArrayExt.cartesProd(input, 2);
  var largestMag = Belt_Array.reduce(candidates, 0, (function (largest, cand) {
          var mag;
          if (cand.length !== 2) {
            mag = -1;
          } else {
            var sfn_a = cand[0];
            var sfn_b = cand[1];
            mag = magnitude(add(sfn_a, sfn_b));
          }
          if (mag > largest) {
            return mag;
          } else {
            return largest;
          }
        }));
  console.log(largestMag);
  
}

function processInput(input) {
  return Belt_Array.map(Helper.Input.toLines(input), read);
}

var data = Helper.Input.read("./src/data/input_day18.txt");

var input = processInput(data);

solve1(input);

solve2(input);

var ex1 = "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n[[[5,[2,8]],4],[5,[[9,9],0]]]\n[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n[[[[5,4],[7,7]],8],[[8,3],8]]\n[[9,3],[[9,9],[6,[4,9]]]]\n[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]";

export {
  SFNum ,
  solve1 ,
  solve2 ,
  processInput ,
  data ,
  ex1 ,
  input ,
  
}
/* data Not a pure module */
