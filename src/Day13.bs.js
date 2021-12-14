// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Helper from "./Helper.bs.js";
import * as Belt_Int from "../node_modules/rescript/lib/es6/belt_Int.js";
import * as Belt_Set from "../node_modules/rescript/lib/es6/belt_Set.js";
import * as Belt_Array from "../node_modules/rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "../node_modules/rescript/lib/es6/belt_Option.js";
import * as Caml_splice_call from "../node_modules/rescript/lib/es6/caml_splice_call.js";

var data = Helper.Input.read("./src/data/input_day13.txt");

function processInput(input) {
  var match = Belt_Array.partition(Helper.Input.toLines(input), (function (line) {
          return line.startsWith("fold");
        }));
  var folds = Belt_Array.map(match[0], (function (line) {
          var match = Helper.ArrayExt.filterEmptyStr(line.trim().split(" "));
          if (match.length !== 3) {
            throw {
                  RE_EXN_ID: "Not_found",
                  Error: new Error()
                };
          }
          var instr = match[2];
          var match$1 = instr.split("=");
          if (match$1.length !== 2) {
            throw {
                  RE_EXN_ID: "Not_found",
                  Error: new Error()
                };
          }
          var axis = match$1[0];
          var coord = match$1[1];
          return [
                  axis,
                  Belt_Option.getExn(Belt_Int.fromString(coord))
                ];
        }));
  var coords = Belt_Array.map(match[1], (function (line) {
          var match = Helper.ArrayExt.toIntArrExn(Helper.ArrayExt.filterEmptyStr(line.trim().split(",")));
          if (match.length !== 2) {
            throw {
                  RE_EXN_ID: "Not_found",
                  Error: new Error()
                };
          }
          var a = match[0];
          var b = match[1];
          return [
                  a,
                  b
                ];
        }));
  return [
          folds,
          coords
        ];
}

var match = processInput(data);

var coords = match[1];

var folds = match[0];

function foldAll(coords, instr) {
  return Belt_Set.toArray(Belt_Set.fromArray(Belt_Array.map(coords, (function (coord) {
                        var y = coord[1];
                        var x = coord[0];
                        switch (instr[0]) {
                          case "x" :
                              var axisX = instr[1];
                              return [
                                      axisX - Math.abs(x - axisX | 0) | 0,
                                      y
                                    ];
                          case "y" :
                              var axisY = instr[1];
                              return [
                                      x,
                                      axisY - Math.abs(y - axisY | 0) | 0
                                    ];
                          default:
                            throw {
                                  RE_EXN_ID: "Not_found",
                                  Error: new Error()
                                };
                        }
                      })), Helper.Tuple.CmpInt));
}

var coordsFolded = foldAll(coords, Belt_Array.getExn(folds, 0));

console.log("First fold dot count: ", coordsFolded.length);

var coordsFoldedComplete = Belt_Array.reduce(folds, coords, foldAll);

function renderGrid(coords) {
  var match = Belt_Array.unzip(coords);
  var maxX = Caml_splice_call.spliceApply(Math.max, [match[0]]);
  var maxY = Caml_splice_call.spliceApply(Math.max, [match[1]]);
  return Belt_Array.forEach(Belt_Array.range(0, maxY), (function (row) {
                console.log(Belt_Array.reduce(Belt_Array.range(0, maxX), "", (function (str, col) {
                            return str + (
                                    Belt_Array.some(coords, (function (param) {
                                            if (param[0] === col) {
                                              return param[1] === row;
                                            } else {
                                              return false;
                                            }
                                          })) ? "#" : "."
                                  );
                          })));
                
              }));
}

console.log("Code:");

renderGrid(coordsFoldedComplete);

var ex1 = "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5";

export {
  data ,
  ex1 ,
  processInput ,
  folds ,
  coords ,
  foldAll ,
  coordsFolded ,
  coordsFoldedComplete ,
  renderGrid ,
  
}
/* data Not a pure module */
