// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Fs from "fs";
import * as Belt_Int from "../node_modules/rescript/lib/es6/belt_Int.js";
import * as Belt_List from "../node_modules/rescript/lib/es6/belt_List.js";
import * as Belt_Array from "../node_modules/rescript/lib/es6/belt_Array.js";
import * as Caml_int32 from "../node_modules/rescript/lib/es6/caml_int32.js";

var raw = Fs.readFileSync("./src/data/input_day6.txt", "utf8");

function make(birthPeriod, param) {
  if (birthPeriod !== undefined) {
    return {
            birthTimer: birthPeriod
          };
  } else {
    return {
            birthTimer: 8
          };
  }
}

function show(fish) {
  return fish.birthTimer;
}

var Lanternfish = {
  firstBirthPeriod: 8,
  nextBirthPeriod: 6,
  make: make,
  show: show
};

function simulatePopulation(_generations, _days) {
  while(true) {
    var days = _days;
    var generations = _generations;
    var advanceGen = function (_currGen, _param) {
      while(true) {
        var param = _param;
        var currGen = _currGen;
        var nextGen = param[1];
        var births = param[0];
        if (!currGen) {
          return Belt_List.concat(births, Belt_List.reverse(nextGen));
        }
        var rest = currGen.tl;
        var fish = currGen.hd;
        if (fish.birthTimer > 0) {
          var fish$1 = {
            birthTimer: fish.birthTimer - 1 | 0
          };
          _param = [
            births,
            Belt_List.add(nextGen, fish$1)
          ];
          _currGen = rest;
          continue ;
        }
        var fish$2 = {
          birthTimer: 6
        };
        _param = [
          Belt_List.add(births, {
                birthTimer: 8
              }),
          Belt_List.add(nextGen, fish$2)
        ];
        _currGen = rest;
        continue ;
      };
    };
    var currGen = Belt_List.head(generations);
    if (currGen === undefined) {
      return generations;
    }
    if (days <= 0) {
      return generations;
    }
    var nextGen = advanceGen(currGen, [
          /* [] */0,
          /* [] */0
        ]);
    _days = days - 1 | 0;
    _generations = Belt_List.add(generations, nextGen);
    continue ;
  };
}

var initGen = Belt_List.fromArray(Belt_Array.reverse(Belt_Array.map(raw.split(","), (function (nStr) {
                return make(Belt_Int.fromString(nStr), undefined);
              }))));

var generations = simulatePopulation({
      hd: initGen,
      tl: /* [] */0
    }, 80);

console.log("Total fish after " + String(80) + " days:", Belt_List.length(Belt_List.head(generations)));

var sum = (function(x,y) { return x + y; });

var fromInt = (function(x) { return BigInt(x); });

var zero = 0n;

var one = 1n;

var Js_bigInt = {
  sum: sum,
  fromInt: fromInt,
  zero: zero,
  one: one
};

function calcBirths(birthCount, days) {
  var cycleDays = 7;
  var daysNextBirths = days - (birthCount + 1 | 0) | 0;
  var births = 1 + Caml_int32.div(daysNextBirths, cycleDays) | 0;
  var remainingDaysOffspring = Belt_Array.makeBy(births, (function (i) {
          return days - ((birthCount + 1 | 0) + Math.imul(i, cycleDays) | 0) | 0;
        }));
  var allDescBirths = Belt_Array.reduce(remainingDaysOffspring, zero, (function (descendants, remDays) {
          var descBirths = remDays > 8 ? calcBirths(8, remDays) : zero;
          return sum(descendants, descBirths);
        }));
  return sum(fromInt(births), allDescBirths);
}

var initGen$1 = Belt_Array.map(raw.split(","), Belt_Int.fromString);

var birthCountResults = Belt_Array.makeBy(5, (function (i) {
        return sum(calcBirths(i + 1 | 0, 256), one);
      }));

var populationSize = Belt_Array.reduce(initGen$1, zero, (function (sum$1, birthCount) {
        return sum(sum$1, birthCountResults[birthCount - 1 | 0]);
      }));

console.log("Total fish after " + String(256) + " days:", populationSize);

var sample = "3,4,3,1,2";

var days = 256;

export {
  raw ,
  sample ,
  Lanternfish ,
  simulatePopulation ,
  generations ,
  Js_bigInt ,
  calcBirths ,
  days ,
  initGen$1 as initGen,
  birthCountResults ,
  populationSize ,
  
}
/* raw Not a pure module */
