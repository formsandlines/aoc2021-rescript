open Belt
open Helper


let processInput = input => {
  /* let (trgBoundsX, trgBoundsY) = */
    switch input->Js.String2.splitByRe(%re("/,*\s[xy]\=/")) {
      | [_, Some(xStr), Some(yStr)] => (
        switch xStr->Js.String2.split("..")->ArrayExt.toIntArrExn {
         | [trgMinY, trgMaxY] => (trgMinY, trgMaxY)
         | _ => raise(Not_found)
         },
        switch yStr->Js.String2.split("..")->ArrayExt.toIntArrExn {
         | [trgMinY, trgMaxY] => (trgMinY, trgMaxY)
         | _ => raise(Not_found)
         })
      | _ => raise(Not_found)
      }
}

module Vec = {
  type t = {x: int, y: int}

  let add = (v1,v2) => {x: v1.x + v2.x, y: v1.y + v2.y}
}

let drag: Vec.t => Vec.t = vel =>
  {x: if vel.x < 0 {1} else if vel.x > 0 {-1} else {0}, y:0}
let grav: Vec.t = {x:0, y:-1}

let move = (pos, vel) => {
  let pos = pos->Vec.add(vel)
  let vel = vel->Vec.add(drag(vel))->Vec.add(grav)
  (pos,vel)
}

let run = (~steps, pos, vel) => {

  let rec aux = (hist, i, pos, vel) => {
    if i > steps { hist }
    else {
      let (pos',vel') = move(pos, vel)

      let hist = hist->Array.concat([pos'])
      hist->aux(i+1, pos', vel')
    }}
  [pos]->aux(1, pos, vel)
}

let getMaxY = (pos, vel) => {

  let rec aux = (i, pos, vel) => {
    if i > 1000 { raise(Not_found) }
    else {
      let (pos',vel') = move(pos, vel)
      Js.log2(i, pos)

      if pos'.y < pos.y { pos.y }
      else {
        aux(i+1, pos', vel')
      }
    }}
  aux(0, pos, vel)
}

let render = (hist, ((trgXMin, trgXMax), (trgYMin, trgYMax))) => {

  let histPoints = hist->Array.map(({x,y}: Vec.t) => (x,y))
    ->Set.fromArray(~id=module(Tuple.CmpInt))

  let ((minX, maxX), (minY, maxY)) =
    switch histPoints->Set.toArray->Array.unzip {
      | (xs, ys) => (
          xs->Tuple.getArrBounds(~minFrom=trgXMin, ~maxFrom=trgXMax),
          ys->Tuple.getArrBounds(~minFrom=trgYMin, ~maxFrom=trgYMax)
        )
      }

  Array.range(minY, maxY)->Array.reverse->Array.forEach(y => Js.log(
      Array.range(minX, maxX)->Array.map(x => {
        if histPoints->Set.has((x,y)) { x == 0 && y == 0 ? "S" : "#" }
        else { x >= trgXMin && y >= trgYMin &&
               x <= trgXMax && y <= trgYMax ? "T" : "." }
      })->Js.Array2.joinWith("") )
    )

}

let solve1 = (targetArea) => {

  let initPos: Vec.t = {x:0, y:0}
  let initVel: Vec.t = {x:16, y:10} // {x:16, y:10}

  /* let maxY = getMaxY(initPos, initVel) */
  /* Js.log(maxY) */

  let hist = run(~steps=30, initPos, initVel)
  hist->Array.forEachWithIndex((i,pos) => Js.log2(i,pos))
  hist->render(targetArea)

}


let data = Input.read("./src/data/input_day17.txt")

let ex1 =
"target area: x=20..30, y=-10..-5"

let targetArea = ex1->processInput
/* let [(xFrom, xTo), (yFrom, yTo)] = ex1->processInput */

solve1(targetArea)
