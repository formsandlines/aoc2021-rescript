// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Helper from "./Helper.bs.js";
import * as Js_int from "../node_modules/rescript/lib/es6/js_int.js";
import * as Belt_Map from "../node_modules/rescript/lib/es6/belt_Map.js";
import * as Belt_List from "../node_modules/rescript/lib/es6/belt_List.js";
import * as Belt_Array from "../node_modules/rescript/lib/es6/belt_Array.js";
import * as Caml_option from "../node_modules/rescript/lib/es6/caml_option.js";
import * as Belt_MapString from "../node_modules/rescript/lib/es6/belt_MapString.js";
import * as Belt_SetString from "../node_modules/rescript/lib/es6/belt_SetString.js";

function processInput(input) {
  var lines = Helper.Input.toLines(input);
  var templ = Belt_List.fromArray(Helper.ArrayExt.filterEmptyStr(Belt_Array.getExn(lines, 0).split("")));
  var rules = Belt_Map.fromArray(Belt_Array.map(lines.slice(1), (function (line) {
              var match = line.split(" -> ");
              if (match.length !== 2) {
                throw {
                      RE_EXN_ID: "Not_found",
                      Error: new Error()
                    };
              }
              var pair = match[0];
              var elem = match[1];
              return [
                      [
                        pair[0],
                        pair[1]
                      ],
                      elem
                    ];
            })), Helper.Tuple.CmpStr);
  return [
          templ,
          rules
        ];
}

function applyRule(a, b, rules) {
  return Belt_Map.get(rules, [
              a,
              b
            ]);
}

function polymerize(templ, rules) {
  var aux = function (_templ, _prevElem, _polymer) {
    while(true) {
      var polymer = _polymer;
      var prevElem = _prevElem;
      var templ = _templ;
      if (!templ) {
        return polymer;
      }
      var elem = templ.hd;
      var x = applyRule(prevElem, elem, rules);
      var polymer$1 = x !== undefined ? Belt_List.add(Belt_List.add(polymer, Caml_option.valFromOption(x)), elem) : Belt_List.add(polymer, elem);
      _polymer = polymer$1;
      _prevElem = elem;
      _templ = templ.tl;
      continue ;
    };
  };
  var firstElem = Belt_List.headExn(templ);
  return Belt_List.reverse(aux(Belt_List.tailExn(templ), firstElem, {
                  hd: firstElem,
                  tl: /* [] */0
                }));
}

function summarize(polymer) {
  var types = Belt_SetString.fromArray(Belt_List.toArray(polymer));
  return Belt_SetString.reduce(types, undefined, (function (stats, tp) {
                return Belt_MapString.set(stats, tp, Belt_List.length(Belt_List.keep(polymer, (function (elem) {
                                      return elem === tp;
                                    }))));
              }));
}

function getStats(summary) {
  return Belt_Array.reduce(Belt_MapString.valuesToArray(summary), [
              Js_int.max,
              0
            ], (function (param, sum) {
                return [
                        Math.min(param[0], sum),
                        Math.max(param[1], sum)
                      ];
              }));
}

function solve1(templ, rules) {
  var polymer = Belt_Array.reduce(Belt_Array.range(1, 10), templ, (function (polymer, param) {
          return polymerize(polymer, rules);
        }));
  var summary = summarize(polymer);
  var match = getStats(summary);
  var mostCommon = match[1];
  var leastCommon = match[0];
  console.log(Belt_MapString.toArray(summary));
  console.log("Least common element: ", leastCommon);
  console.log("Most common element: ", mostCommon);
  console.log("Solution: ", mostCommon - leastCommon | 0);
  
}

function solve2(templ, rules) {
  var polymer = Belt_Array.reduce(Belt_Array.range(1, 20), templ, (function (polymer, param) {
          return polymerize(polymer, rules);
        }));
  var summary = summarize(polymer);
  var match = getStats(summary);
  var mostCommon = match[1];
  var leastCommon = match[0];
  console.log(Belt_MapString.toArray(summary));
  console.log("Least common element: ", leastCommon);
  console.log("Most common element: ", mostCommon);
  console.log("Solution: ", mostCommon - leastCommon | 0);
  
}

var data = Helper.Input.read("./src/data/input_day14.txt");

var match = processInput(data);

var pairInsertRules = match[1];

var polymerTempl = match[0];

console.log(Belt_List.toArray(polymerTempl), Belt_Map.toArray(pairInsertRules));

solve1(polymerTempl, pairInsertRules);

solve2(polymerTempl, pairInsertRules);

var ex1 = "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C";

export {
  processInput ,
  applyRule ,
  polymerize ,
  summarize ,
  getStats ,
  solve1 ,
  solve2 ,
  data ,
  ex1 ,
  polymerTempl ,
  pairInsertRules ,
  
}
/* data Not a pure module */
