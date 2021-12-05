let raw = Node_fs.readFileSync("./src/data/input_day5.txt", #utf8)

let sample = `0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2`

module Point = {
  type t = (int, int)

  let eq: (t,t) => bool
    = ((x,y), (x',y')) => (x == x') && (y == y')

  module PointComparator =
    Belt.Id.MakeComparable({
      type t = (int, int)
      let cmp = ((a0, a1), (b0, b1)) =>
        switch (Pervasives.compare(a0, b0)) {
        | 0 => Pervasives.compare(a1, b1)
        | c => c
        }
    })

  let makeSet = points => {
    points->Belt.Set.fromArray(~id=module(PointComparator))
  }

}

module Line = {
  type t = (Point.t, Point.t)

  let interpolateLine: t => array<Point.t>
    = ( ((x1,y1),(x2,y2)) ) => {

    let interpolateDist = (a, b) => {
      if b - a >= 0 {
        Belt.Array.range(a, b) // ascending order
      } else {
        Belt.Array.range(b, a)->Belt.Array.reverse // descending order
      }
    }

    let xs = interpolateDist(x1, x2)
    let ys = interpolateDist(y1, y2)

    switch (xs, ys) {
      | ([x],[y]) => [(x,y)] // both Points are equal
      | ( _ ,[y]) => xs->Belt.Array.map(x => (x,y))
      | ([x], _ ) => ys->Belt.Array.map(y => (x,y))
      | ( _ , _ ) => 
        if (xs->Belt.Array.length == ys->Belt.Array.length) {
          Belt.Array.zip(xs, ys)
        } else {
          raise(Not_found) // diagonal not 45°
        }
    }
  }

  let countIntersections = lines => {

    let countPointOverlaps = points => {
      let uniquePoints = points->Point.makeSet

      uniquePoints->Belt.Set.keep(p => {
        let count = ref(-1)
        points->Belt.Array.some(p' => {
          if Point.eq(p, p') {
            count := count.contents + 1
            count.contents > 0
          } else { false }
        })
      })
      ->Belt.Set.size
    }

    let interPoints = lines
      ->Belt.Array.map(l => l->interpolateLine)
      ->Belt.Array.concatMany

    interPoints->countPointOverlaps
  }

  let isDiagonal = ( ((x1,y1),(x2,y2)) ) =>
    (x1 != x2) && (y1 != y2)
}


// Part 1 *

let ventLines: array<Line.t> = raw->Js.String2.split("\n")
  ->Belt.Array.keep(str => str != "")
  ->Belt.Array.map(str => {
    let pairs = str->Js.String2.split(" -> ")
      ->Belt.Array.map(str => {
        let nums = str->Js.String2.split(",")
          ->Belt.Array.map(ch => ch->Belt.Int.fromString->Belt.Option.getExn)
        (nums[0], nums[1]) 
      })
    (pairs[0], pairs[1])
  })

let ventLines_orthogonal = ventLines
  ->Belt.Array.keep(l => !(l->Line.isDiagonal))

let overlaps = ventLines_orthogonal->Line.countIntersections

Js.log2("Part 1 / Overlaps (orthogonal): ", overlaps)


// Part 2 *

let overlaps = ventLines->Line.countIntersections

Js.log2("Part 2 / Overlaps (all): ", overlaps)
