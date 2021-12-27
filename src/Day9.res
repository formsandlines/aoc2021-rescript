open Belt
open Helper

/** Collects points that bave heights lower than all their adjacent points */
let collectLowPoints = heightmap => heightmap
  ->Array.reduceWithIndex(list{}, (lows, heightRow, y) => lows
    ->List.concat(heightRow->Array.reduceWithIndex(list{}, (lowsRow, height, x) => {
      let neighbours = [
        heightRow->Array.get(x-1), // left
        heightRow->Array.get(x+1), // right
        heightmap->Array.get(y-1)->Option.flatMap(arr => arr->Array.get(x)), // up
        heightmap->Array.get(y+1)->Option.flatMap(arr => arr->Array.get(x))  // down
      ]->Array.keep(opt => opt != None)->Array.map(opt => opt->Option.getExn)

      if neighbours->Array.every(nbHeight => height < nbHeight) {
        lowsRow->List.add(height)
      } else { lowsRow }
    })))

let getAdjacentCoords = ((x,y), heightmap, (maxX, maxY)) => Set.fromArray( 
    [(x-1,y),(x+1,y),(x,y-1),(x,y+1)], ~id=module(Tuple.CmpInt)
  )->Set.keep(((sx,sy)) => {
    let inBounds = sx >= 0 && sx <= maxX && sy >= 0 && sy <= maxY
    if inBounds {
      heightmap->Array.getExn(sy)->Array.getExn(sx) != 9
    } else { false }
  })

/** Integrates coordinate-set into a basin if they intersect */
let updateBasins = (basins, coords) => {
  let rec aux = (basins, basins') => switch basins {
    | list{} => (basins', false)
    | list{basin, ...r} =>
        if !Set.isEmpty(Set.intersect(coords, basin)) {
          (basins'->List.concat(list{basin->Set.union(coords), ...r}), true)
        } else {
          r->aux(basins'->List.add(basin))
        }
    }
  basins->aux(list{})
}

/** Reduces basin list if a basin can be merged into another */
let rec mergeBasins  = basins => switch basins {
  | list{} => list{}
  | list{basin, ...r} => switch r->updateBasins(basin) {
      | (basins', true) => basins' // only merges once to increase performance
      | _ => list{ basin, ...r->mergeBasins }
      }
  }

/** Collects adjacent coordinates from the height map in basin sets */
let collectBasins = heightmap => {
  let maxX = heightmap->Array.getExn(0)->Array.length - 1
  let maxY = heightmap->Array.length - 1

  // map each height cell to a set of its neighbour coordinates (+ its own)
  let allAdjCoords = heightmap
    ->Array.reduceWithIndex([], (adjCoords, heightRow, y) => heightRow
      ->Array.mapWithIndex((x, height) => if height != 9 {
          (x,y)->getAdjacentCoords(heightmap, (maxX, maxY))->Set.add((x,y))
        } else { Set.make(~id=module(Tuple.CmpInt)) })
      ->Array.keep(s => s->Set.size > 0)
      ->Array.concat(adjCoords, _)
  )

  // merge coordinate sets that have adjacent coords into contiguous basin sets
  allAdjCoords->Array.reduce(list{}, (basins, coords) => {
      // first, try to integrate the coords into a previously created basin set
      let (basins, updated) = basins->updateBasins(coords)
      // if integration happened, a previously created basin may now have an
      // adjacent basin, in which case they must be merged
      if updated { basins->mergeBasins } else { basins->List.add(coords) }
    })
}

/** Maps each coordinate of the height map to its basin area and renders it */
let renderBasins = (heightmap, basins) => {
  let basinmap = heightmap->Array.mapWithIndex((y,heights) => heights
    ->Array.mapWithIndex((x,_) => {
      let idxs = basins->List.reduceWithIndex(list{},(idxs,basin,i) =>
        if basin->Set.has((x,y)) { idxs->List.add(i) } else { idxs })

      // picks distinct unicode characters for each basin (from Braille codes)
      let pickChar = n => Js.String2.fromCharCode(n+10241)
      switch idxs {
        | list{} => ` `
        | list{i} => i->pickChar //`▒`
        | _ => `X` // <- basins not mutually exclusive, should never be shown!
      }
    })->Js.Array2.joinWith(""))->Js.Array2.joinWith(`\n`)

  Js.log(basinmap)
}


let part1 = heightmap => { // *

  let lowPoints = collectLowPoints(heightmap)

  let riskLevels = lowPoints->List.map(\"+"(1))->List.toArray
  let result = riskLevels->Array.reduce(0, \"+")

  Js.log2("Risk levels:",riskLevels)
  Js.log2("Result:",result)
}

let part2 = heightmap => { // *

  let basins = heightmap->collectBasins

  // for debugging:
  /* Js.log(basins->List.toArray->Array.map(basin => basin->Set.toArray)) */
  /* Js.log(renderBasins(heightmap, basins)) */ 

  let basinSizes = basins->List.toArray->Array.map(basin => basin->Set.size)
    ->SortArray.Int.stableSort

  let largest3 = basinSizes->Array.sliceToEnd(-3)
  Js.log2("Size of largest 3 basins:",largest3)

  let result = largest3->Array.reduce(1, \"*")
  Js.log2("Result:",result)
}


let data = Input.read("./src/data/input_day9.txt")

let ex1 = "
2199943210
3987894921
9856789892
8767896789
9899965678"

let ex2 = "
1293
0920
9191
9012"

let processInput = input => input->Input.toLines
  ->Array.map(line => line->Js.String2.trim
    ->Js.String2.split("")->ArrayExt.filterEmptyStr
    ->ArrayExt.toIntArrExn)

let input = data->processInput

ignore( input->part1 )
ignore( input->part2 ) // slow to compute
