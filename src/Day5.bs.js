// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Fs from "fs";
import * as Belt_Id from "../node_modules/rescript/lib/es6/belt_Id.js";
import * as Belt_Int from "../node_modules/rescript/lib/es6/belt_Int.js";
import * as Belt_Set from "../node_modules/rescript/lib/es6/belt_Set.js";
import * as Caml_obj from "../node_modules/rescript/lib/es6/caml_obj.js";
import * as Belt_Array from "../node_modules/rescript/lib/es6/belt_Array.js";
import * as Caml_array from "../node_modules/rescript/lib/es6/caml_array.js";
import * as Belt_Option from "../node_modules/rescript/lib/es6/belt_Option.js";

var raw = Fs.readFileSync("./src/data/input_day5.txt", "utf8");

function eq(param, param$1) {
  if (param[0] === param$1[0]) {
    return param[1] === param$1[1];
  } else {
    return false;
  }
}

function cmp(param, param$1) {
  var c = Caml_obj.caml_compare(param[0], param$1[0]);
  if (c !== 0) {
    return c;
  } else {
    return Caml_obj.caml_compare(param[1], param$1[1]);
  }
}

var PointComparator = Belt_Id.MakeComparable({
      cmp: cmp
    });

function makeSet(points) {
  return Belt_Set.fromArray(points, PointComparator);
}

var Point = {
  eq: eq,
  PointComparator: PointComparator,
  makeSet: makeSet
};

function interpolateLine(param) {
  var match = param[1];
  var match$1 = param[0];
  var interpolateDist = function (a, b) {
    if ((b - a | 0) >= 0) {
      return Belt_Array.range(a, b);
    } else {
      return Belt_Array.reverse(Belt_Array.range(b, a));
    }
  };
  var xs = interpolateDist(match$1[0], match[0]);
  var ys = interpolateDist(match$1[1], match[1]);
  if (xs.length === 1) {
    var x = xs[0];
    if (ys.length === 1) {
      var y = ys[0];
      return [[
                x,
                y
              ]];
    }
    
  }
  if (ys.length !== 1) {
    if (xs.length !== 1) {
      if (xs.length === ys.length) {
        return Belt_Array.zip(xs, ys);
      }
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    }
    var x$1 = xs[0];
    return Belt_Array.map(ys, (function (y) {
                  return [
                          x$1,
                          y
                        ];
                }));
  }
  var y$1 = ys[0];
  return Belt_Array.map(xs, (function (x) {
                return [
                        x,
                        y$1
                      ];
              }));
}

function countIntersections(lines) {
  var points = Belt_Array.concatMany(Belt_Array.map(lines, interpolateLine));
  var uniquePoints = Belt_Set.fromArray(points, PointComparator);
  return Belt_Set.size(Belt_Set.keep(uniquePoints, (function (p) {
                    var count = {
                      contents: -1
                    };
                    return Belt_Array.some(points, (function (p$p) {
                                  if (eq(p, p$p)) {
                                    count.contents = count.contents + 1 | 0;
                                    return count.contents > 0;
                                  } else {
                                    return false;
                                  }
                                }));
                  })));
}

function isDiagonal(param) {
  var match = param[1];
  var match$1 = param[0];
  if (Caml_obj.caml_notequal(match$1[0], match[0])) {
    return Caml_obj.caml_notequal(match$1[1], match[1]);
  } else {
    return false;
  }
}

var Line = {
  interpolateLine: interpolateLine,
  countIntersections: countIntersections,
  isDiagonal: isDiagonal
};

var ventLines = Belt_Array.map(Belt_Array.keep(raw.split("\n"), (function (str) {
            return str !== "";
          })), (function (str) {
        var pairs = Belt_Array.map(str.split(" -> "), (function (str) {
                var nums = Belt_Array.map(str.split(","), (function (ch) {
                        return Belt_Option.getExn(Belt_Int.fromString(ch));
                      }));
                return [
                        Caml_array.get(nums, 0),
                        Caml_array.get(nums, 1)
                      ];
              }));
        return [
                Caml_array.get(pairs, 0),
                Caml_array.get(pairs, 1)
              ];
      }));

var ventLines_orthogonal = Belt_Array.keep(ventLines, (function (l) {
        return !isDiagonal(l);
      }));

var overlaps = countIntersections(ventLines_orthogonal);

console.log("Part 1 / Overlaps (orthogonal): ", overlaps);

var overlaps$1 = countIntersections(ventLines);

console.log("Part 2 / Overlaps (all): ", overlaps$1);

var sample = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2";

export {
  raw ,
  sample ,
  Point ,
  Line ,
  ventLines ,
  ventLines_orthogonal ,
  overlaps$1 as overlaps,
  
}
/* raw Not a pure module */