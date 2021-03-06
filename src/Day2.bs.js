// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Fs from "fs";
import * as Belt_Int from "../node_modules/rescript/lib/es6/belt_Int.js";
import * as Belt_List from "../node_modules/rescript/lib/es6/belt_List.js";
import * as Belt_Array from "../node_modules/rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "../node_modules/rescript/lib/es6/belt_Option.js";

var raw = Fs.readFileSync("./src/data/input_day2.txt", "utf8");

function makeInstr(dirStr, val) {
  switch (dirStr) {
    case "down" :
        return {
                TAG: /* Down */1,
                _0: val
              };
    case "forward" :
        return {
                TAG: /* Forward */0,
                _0: val
              };
    case "up" :
        return {
                TAG: /* Up */2,
                _0: val
              };
    default:
      return ;
  }
}

var data = Belt_List.fromArray(Belt_Array.mapU(Belt_Array.keep(raw.split("\n"), (function (str) {
                return str !== "";
              })), (function (str) {
            var match = str.split(" ");
            var tmp;
            if (match.length !== 2) {
              tmp = undefined;
            } else {
              var str$1 = match[0];
              var n = match[1];
              tmp = makeInstr(str$1, Belt_Option.getExn(Belt_Int.fromString(n)));
            }
            return Belt_Option.getExn(tmp);
          })));

function runInstr(_data, _pos) {
  while(true) {
    var pos = _pos;
    var data = _data;
    if (!data) {
      return pos;
    }
    var n = data.hd;
    switch (n.TAG | 0) {
      case /* Forward */0 :
          _pos = {
            horiz: pos.horiz + n._0 | 0,
            depth: pos.depth
          };
          _data = data.tl;
          continue ;
      case /* Down */1 :
          _pos = {
            horiz: pos.horiz,
            depth: pos.depth + n._0 | 0
          };
          _data = data.tl;
          continue ;
      case /* Up */2 :
          _pos = {
            horiz: pos.horiz,
            depth: pos.depth - n._0 | 0
          };
          _data = data.tl;
          continue ;
      
    }
  };
}

var newPos = runInstr(data, {
      horiz: 0,
      depth: 0
    });

var result = Math.imul(newPos.horiz, newPos.depth);

console.log(newPos, result);

function runInstr$1(_data, _pos) {
  while(true) {
    var pos = _pos;
    var data = _data;
    if (!data) {
      return pos;
    }
    var n = data.hd;
    switch (n.TAG | 0) {
      case /* Forward */0 :
          var n$1 = n._0;
          _pos = {
            horiz: pos.horiz + n$1 | 0,
            depth: pos.depth + Math.imul(pos.aim, n$1) | 0,
            aim: pos.aim
          };
          _data = data.tl;
          continue ;
      case /* Down */1 :
          _pos = {
            horiz: pos.horiz,
            depth: pos.depth,
            aim: pos.aim + n._0 | 0
          };
          _data = data.tl;
          continue ;
      case /* Up */2 :
          _pos = {
            horiz: pos.horiz,
            depth: pos.depth,
            aim: pos.aim - n._0 | 0
          };
          _data = data.tl;
          continue ;
      
    }
  };
}

var newPos$1 = runInstr$1(data, {
      horiz: 0,
      depth: 0,
      aim: 0
    });

var result$1 = Math.imul(newPos$1.horiz, newPos$1.depth);

console.log(newPos$1, result$1);

var sample = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2";

export {
  raw ,
  sample ,
  makeInstr ,
  data ,
  runInstr$1 as runInstr,
  newPos$1 as newPos,
  result$1 as result,
  
}
/* raw Not a pure module */
