open Belt
open Helper

let isLowerCase = str => {
  let firstChar = str->Js.String2.get(0)
  firstChar == firstChar->Js.String2.toLowerCase
}

let keepByNode = (arr, node) => arr
  ->Array.keep(((n1, n2)) => n1 == node || n2 == node)

let discardByNode = (arr, node) => arr
  ->Array.keep(((n1, n2)) => n1 != node && n2 != node)

let genPathTree = (links, startNode, endNode, filterFn) => {

  let rec aux = (links, (visited, hadSndVisit), fromNode, toNode) => {
    // get all links that contain the fromNode
    let fromLinks = links->keepByNode(fromNode)

    // for the next passing, filter out visited lowercase nodes
    let (links, hadSndVisit, visited) = if fromNode->isLowerCase {
      // we keep a list of them to detect duplicates for part 2
      let visited = list{ fromNode, ...visited }
      let (links, hadSndVisit) = links->filterFn(fromNode, visited, hadSndVisit)
      (links, hadSndVisit, visited)
    } else { (links, hadSndVisit, visited) }

    // map each start link to the path from the end of its next link to toNode
    fromLinks->Array.reduce([], (paths, (a,b)) => {
      let succ = a == fromNode ? b : a

      if succ == toNode { paths->Array.concat([ ArrayTree.Branch(succ, []) ])
      } else {
        paths->Array.concat([
          ArrayTree.Branch(succ, links->aux((visited, hadSndVisit), succ, toNode) )])
      } })
  }
  ArrayTree.Branch(startNode, links->aux((list{}, false), startNode, endNode) )
}


let part1 = links => { // *

  let filterVisitedSmallCaves = (links, lastNode, _, hadSndVisit) =>
    (links->discardByNode(lastNode), hadSndVisit)

  let pathTree = links->genPathTree("start", "end", filterVisitedSmallCaves)

  /* Js.log(pathTree->ArrayTree.show(x => x)) */


  let pathIndexes = pathTree->ArrayTree.findPaths("end")
    ->Array.map(index => index->List.reverse)

  /* Js.log2("Path Indexes: ", pathIndexes->Array.map(index => index->List.toArray)) */


  let paths = pathIndexes->Array.map(index => index->ArrayTree.getPathFromIndex(pathTree) )

  Js.log2("Paths:", paths->Array.map(path => path->List.toArray->Js.Array2.joinWith(",")))

  Js.log2("Path count:", paths->Array.length)
}


let part2 = links => { // *

  let hasDuplicate = lst => {
    let uniques = lst->List.toArray->Set.String.fromArray
    lst->List.length > uniques->Set.String.size
  }

  let filterVisitedSmallCaves = (links, lastNode, visited, hadSndVisit) =>
    if lastNode == "start" {
      // start nodes must be filtered out on first visit
      (links->discardByNode(lastNode), hadSndVisit)
    } else {
      let hadSndVisit = if !hadSndVisit { visited->hasDuplicate } else { true }
      // visited small caves will be filtered only after a second visit occurred
      let links = if hadSndVisit {
          links->Array.keep(((n1, n2)) => {
              let id = (a, b) => a == b
              if (n1->isLowerCase && List.has(visited, n1, id)) ||
                 (n2->isLowerCase && List.has(visited, n2, id)) {
                false
              } else { true }
            })
        } else { links }
      (links, hadSndVisit)
    }

  let pathTree = links->genPathTree("start", "end", filterVisitedSmallCaves)

  /* Js.log(pathTree->ArrayTree.show(x => x)) */


  let pathIndexes = pathTree->ArrayTree.findPaths("end")
    ->Array.map(index => index->List.reverse)

  /* Js.log2("Path Indexes: ", pathIndexes->Array.map(index => index->List.toArray)) */


  let paths = pathIndexes->Array.map(index => index->ArrayTree.getPathFromIndex(pathTree) )

  Js.log2("Paths:", paths->Array.map(path => path->List.toArray->Js.Array2.joinWith(",")))

  Js.log2("Path count:", paths->Array.length)
}


let data = Input.read("./src/data/input_day12.txt")

let ex1 =
"start-A
start-b
A-c
A-b
b-d
A-end
b-end"

let ex2 =
"dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"

let ex3 =
"fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"

let processInput = input => {
  input->Input.toLines
    ->Array.map(line => switch line->Js.String2.trim->Js.String2.split("-") {
      | [a,b] => (a,b)
      | _ => raise(Not_found)
    })
}

let links = processInput(data)
/* Js.log(links) */

ignore( links->part1 )
ignore( links->part2 )
