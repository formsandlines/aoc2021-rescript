open Belt
open Helper

let data = Input.read("./src/data/input_day11.txt")

let sample =
"5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"

let octoLevels = data->Input.toLines
  ->Array.map(line => line->Js.String2.trim
    ->Js.String2.split("")->ArrayExt.filterEmptyStr
    ->ArrayExt.toIntArrExn)


// I decided to take a more procedural approach this time:

// for 100 steps
  // increment +1 on all energy levels

  // while there are levels > 9
    // map each of them:
      // collect all 8 adjacent (x,y) indexes for processing later
      // increment global flash counter +1
      // set level back to 0

    // for each of the collected adjacent indexes:
      // if not already at 0, increment their level +1

  // continue to next step

let runSimulation = (data, maxSteps) => {
  let states = ref(data->Array.map(line => line->Array.copy))
  let totalFlashes = ref(0)
  let firstSynchronize = ref(None)

  for step in 1 to maxSteps {
    states := states.contents->Array.map(row => row->Array.map(\"+"(1)))

    while states.contents->Array.some(row => row->Array.some(energy => energy > 9)) {
      let toUpdate = []

      states := states.contents->Array.mapWithIndex((y,row) => row
        ->Array.mapWithIndex((x,energy) => if energy > 9 {
          let adjacent = [
            (x-1,y-1),(x,y-1),(x+1,y-1),
            (x-1,y  ),        (x+1,y  ),
            (x-1,y+1),(x,y+1),(x+1,y+1)]

          adjacent->Array.forEach(((x',y')) =>
            states.contents->Array.get(y')->Option.flatMap(row => row
              ->Array.get(x'))->Option.forEach(_ => {
                let _ = Js.Array2.push(toUpdate, (x',y'))
                ()
              }))
          
          totalFlashes := totalFlashes.contents + 1
          0
        } else { energy }))

      toUpdate->Array.forEach(((x,y)) => {
        let row = states.contents->Array.getExn(y)
        let energy = row->Array.getExn(x)

        row->Array.setExn(x, if energy > 0 { energy + 1 } else { energy })
      })


      // determine if all octopuses flash simultaneously
      firstSynchronize := switch firstSynchronize.contents {
        | None => (states.contents->Array.every(row => row
            ->Array.every(energy => energy == 0)) ? Some(step) : None)
        | Some(x) => Some(x)
      }
    }
  }

  (totalFlashes.contents, firstSynchronize.contents)
}

let (totalFlashes, _) = octoLevels->runSimulation(100)


// Part 1 *

Js.log(totalFlashes)


// Part 2 *

let (_, firstSynchronize) = octoLevels->runSimulation(1000)

Js.log(firstSynchronize)

