open Model

let touch = (board: board): board => {...board, updatedAt: Date.now()}

let findShape = (board: board, id: string): option<shape> =>
  board.shapes->Array.find(shape => shape.id == id)

let findEdge = (board: board, id: string): option<edge> =>
  board.edges->Array.find(edge => edge.id == id)

let updateShape = (board: board, id: string, update: shape => shape): board =>
  touch({
    ...board,
    shapes: board.shapes->Array.map(shape => shape.id == id ? update(shape) : shape),
  })

let updateEdge = (board: board, id: string, update: edge => edge): board =>
  touch({
    ...board,
    edges: board.edges->Array.map(edge => edge.id == id ? update(edge) : edge),
  })

let addShape = (board: board, shape: shape): board =>
  touch({...board, shapes: board.shapes->Array.concat([shape])})

let addEdge = (board: board, edge: edge): board => {
  let duplicate =
    board.edges->Array.some(item =>
      (item.from == edge.from && item.to_ == edge.to_) ||
        (!edge.directed && item.from == edge.to_ && item.to_ == edge.from)
    )
  duplicate ? board : touch({...board, edges: board.edges->Array.concat([edge])})
}

let deleteSelection = (board: board, selection: selection): board =>
  switch selection {
  | NoSelection => board
  | ShapeSelection(id) =>
    touch({
      ...board,
      shapes: board.shapes->Array.filter(shape => shape.id != id),
      edges: board.edges->Array.filter(edge => edge.from != id && edge.to_ != id),
    })
  | EdgeSelection(id) => touch({...board, edges: board.edges->Array.filter(edge => edge.id != id)})
  }

let rename = (board: board, name: string): board =>
  touch({...board, name: name->String.trim == "" ? "Untitled board" : name->String.trim})

let setAllFontSizes = (board: board, fontSize: int): board =>
  touch({
    ...board,
    shapes: board.shapes->Array.map(shape => {...shape, fontSize}),
    edges: board.edges->Array.map(edge => {...edge, fontSize}),
  })

let addLayer = (board: board): (board, int) => {
  let index = board.layers->Array.length
  let layer = {
    id: Model.uid(),
    name: "Layer " ++ index->Int.toString,
    hidden: false,
    color: "#38bdf8",
  }
  (touch({...board, layers: board.layers->Array.concat([layer])}), index)
}

let updateLayer = (board: board, index: int, update: layer => layer): board =>
  touch({
    ...board,
    layers: board.layers->Array.mapWithIndex((item, itemIndex) =>
      itemIndex == index ? update(item) : item
    ),
  })

let deleteLayer = (board: board, index: int, ~purge=false): board => {
  if board.layers->Array.length <= 1 {
    board
  } else {
    let nextShapes = purge
      ? board.shapes->Array.filter(shape => shape.layer != index)
      : board.shapes
        ->Array.filter(shape => shape.layer != index || index > 0)
        ->Array.map(shape => {
          if shape.layer == index {
            {...shape, layer: index > 0 ? index - 1 : 0}
          } else if shape.layer > index {
            {...shape, layer: shape.layer - 1}
          } else {
            shape
          }
        })
    let validIds = nextShapes->Array.map(shape => shape.id)
    touch({
      ...board,
      shapes: nextShapes,
      edges: board.edges->Array.filter(edge =>
        validIds->Array.includes(edge.from) && validIds->Array.includes(edge.to_)
      ),
      layers: board.layers->Array.filterWithIndex((_layer, itemIndex) => itemIndex != index),
    })
  }
}

let moveSelectionToLayer = (board: board, selection: selection, layer: int): board =>
  switch selection {
  | ShapeSelection(id) => updateShape(board, id, shape => {...shape, layer})
  | _ => board
  }

let autoLayout = (board: board): board => {
  let count = board.shapes->Array.length
  if count == 0 {
    board
  } else {
    let columns = Math.ceil(count->Int.toFloat->Math.sqrt)->Float.toInt
    let rows = Math.ceil(count->Int.toFloat /. columns->Int.toFloat)->Float.toInt
    let width = 610.0
    let height = 240.0
    let nextShapes = board.shapes->Array.mapWithIndex((shape, index) => {
      let column = index % columns
      let row = index / columns
      {
        ...shape,
        x: (column->Int.toFloat -. (columns - 1)->Int.toFloat /. 2.0) *. width -. shape.w /. 2.0,
        y: (row->Int.toFloat -. (rows - 1)->Int.toFloat /. 2.0) *. height -. shape.h /. 2.0,
      }
    })
    touch({...board, shapes: nextShapes})
  }
}

let layerItemCount = (board: board, index: int): int =>
  board.shapes->Array.reduce(0, (count, shape) => shape.layer == index ? count + 1 : count)

let clampZoom = zoom => Math.max(0.2, Math.min(4.0, zoom))
