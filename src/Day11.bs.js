// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Helper from "./Helper.bs.js";
import * as Belt_Array from "../node_modules/rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "../node_modules/rescript/lib/es6/belt_Option.js";

var data = Helper.Input.read("./src/data/input_day11.txt");

var octoLevels = Belt_Array.map(Helper.Input.toLines(data), (function (line) {
        return Helper.ArrayExt.toIntArrExn(Helper.ArrayExt.filterEmptyStr(line.trim().split("")));
      }));

function runSimulation(data, maxSteps) {
  var states = {
    contents: Belt_Array.map(data, (function (line) {
            return line.slice(0);
          }))
  };
  var totalFlashes = {
    contents: 0
  };
  var firstSynchronize;
  for(var step = 1; step <= maxSteps; ++step){
    states.contents = Belt_Array.map(states.contents, (function (row) {
            return Belt_Array.map(row, (function (param) {
                          return 1 + param | 0;
                        }));
          }));
    while(Belt_Array.some(states.contents, (function (row) {
              return Belt_Array.some(row, (function (energy) {
                            return energy > 9;
                          }));
            }))) {
      var toUpdate = [];
      states.contents = Belt_Array.mapWithIndex(states.contents, (function(toUpdate){
          return function (y, row) {
            return Belt_Array.mapWithIndex(row, (function (x, energy) {
                          if (energy <= 9) {
                            return energy;
                          }
                          var adjacent = [
                            [
                              x - 1 | 0,
                              y - 1 | 0
                            ],
                            [
                              x,
                              y - 1 | 0
                            ],
                            [
                              x + 1 | 0,
                              y - 1 | 0
                            ],
                            [
                              x - 1 | 0,
                              y
                            ],
                            [
                              x + 1 | 0,
                              y
                            ],
                            [
                              x - 1 | 0,
                              y + 1 | 0
                            ],
                            [
                              x,
                              y + 1 | 0
                            ],
                            [
                              x + 1 | 0,
                              y + 1 | 0
                            ]
                          ];
                          Belt_Array.forEach(adjacent, (function (param) {
                                  var y$p = param[1];
                                  var x$p = param[0];
                                  return Belt_Option.forEach(Belt_Option.flatMap(Belt_Array.get(states.contents, y$p), (function (row) {
                                                    return Belt_Array.get(row, x$p);
                                                  })), (function (param) {
                                                toUpdate.push([
                                                      x$p,
                                                      y$p
                                                    ]);
                                                
                                              }));
                                }));
                          totalFlashes.contents = totalFlashes.contents + 1 | 0;
                          return 0;
                        }));
          }
          }(toUpdate)));
      Belt_Array.forEach(toUpdate, (function (param) {
              var x = param[0];
              var row = Belt_Array.getExn(states.contents, param[1]);
              var energy = Belt_Array.getExn(row, x);
              return Belt_Array.setExn(row, x, energy > 0 ? energy + 1 | 0 : energy);
            }));
      var x = firstSynchronize;
      firstSynchronize = x !== undefined ? x : (
          Belt_Array.every(states.contents, (function (row) {
                  return Belt_Array.every(row, (function (energy) {
                                return energy === 0;
                              }));
                })) ? step : undefined
        );
    };
  }
  return [
          totalFlashes.contents,
          firstSynchronize
        ];
}

var match = runSimulation(octoLevels, 100);

var totalFlashes = match[0];

console.log(totalFlashes);

var match$1 = runSimulation(octoLevels, 1000);

var firstSynchronize = match$1[1];

console.log(firstSynchronize);

var sample = "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526";

export {
  data ,
  sample ,
  octoLevels ,
  runSimulation ,
  totalFlashes ,
  firstSynchronize ,
  
}
/* data Not a pure module */
