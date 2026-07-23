let failures = ref(0)

let check = (name, condition) => {
  if condition {
    Console.log("✓ " ++ name)
  } else {
    failures.contents = failures.contents + 1
    Console.error("✗ " ++ name)
  }
}

let starter = Model.starterBoard()
check("starter board has two shapes", starter.shapes->Array.length == 2)
check("starter board has one edge", starter.edges->Array.length == 1)

let extra = Model.makeShape(~kind="circle", ~x=0.0, ~y=0.0, ())
let withExtra = BoardOps.addShape(starter, extra)
check("add shape", withExtra.shapes->Array.length == 3)

let withoutExtra = BoardOps.deleteSelection(withExtra, ShapeSelection(extra.id))
check("delete selected shape", withoutExtra.shapes->Array.length == 2)

let connectedExtra = BoardOps.addEdge(
  withExtra,
  Model.makeEdge(~from=Array.getUnsafe(starter.shapes, 0).id, ~to_=extra.id, ~directed=true, ()),
)
check("add directed edge", connectedExtra.edges->Array.length == 2)
let duplicateEdge = BoardOps.addEdge(
  connectedExtra,
  Model.makeEdge(~from=Array.getUnsafe(starter.shapes, 0).id, ~to_=extra.id, ~directed=true, ()),
)
check("reject duplicate edge", duplicateEdge.edges->Array.length == 2)
let removedConnected = BoardOps.deleteSelection(connectedExtra, ShapeSelection(extra.id))
check(
  "delete shape removes incident edge",
  removedConnected.shapes->Array.length == 2 && removedConnected.edges->Array.length == 1,
)

let firstEdge = Array.getUnsafe(starter.edges, 0)
let bent = BoardOps.updateEdge(starter, firstEdge.id, edge => {
  ...edge,
  cx: Some(12.0),
  cy: Some(24.0),
})
check(
  "connector bend persists in model",
  switch BoardOps.findEdge(bent, firstEdge.id) {
  | Some(edge) => edge.cx == Some(12.0) && edge.cy == Some(24.0)
  | None => false
  },
)

let renamed = BoardOps.rename(starter, "  Architecture  ")
check("trim board name", renamed.name == "Architecture")

let (layered, newLayer) = BoardOps.addLayer(starter)
check("add layer", layered.layers->Array.length == 2 && newLayer == 1)
let assigned = BoardOps.moveSelectionToLayer(
  layered,
  ShapeSelection(Array.getUnsafe(layered.shapes, 0).id),
  1,
)
check(
  "assign shape to layer",
  Array.getUnsafe(assigned.shapes, 0).layer == 1 && BoardOps.layerItemCount(assigned, 1) == 1,
)
let droppedLayer = BoardOps.deleteLayer(assigned, 1)
check(
  "delete layer drops shape to ground",
  droppedLayer.layers->Array.length == 1 && Array.getUnsafe(droppedLayer.shapes, 0).layer == 0,
)

let resizedText = BoardOps.setAllFontSizes(starter, 32)
check(
  "global text size updates shapes and edges",
  resizedText.shapes->Array.every(shape => shape.fontSize == 32) &&
    resizedText.edges->Array.every(edge => edge.fontSize == 32),
)

let laidOut = BoardOps.autoLayout(withExtra)
check(
  "auto layout changes positions",
  laidOut.shapes->Array.some(shape =>
    switch BoardOps.findShape(withExtra, shape.id) {
    | Some(before) => before.x != shape.x || before.y != shape.y
    | None => false
    }
  ),
)

check("zoom lower bound", BoardOps.clampZoom(0.01) == 0.2)
check("zoom upper bound", BoardOps.clampZoom(9.0) == 4.0)

let projected = Perspective.project(
  ~x=150.0,
  ~y=300.0,
  ~view=Model.defaultView,
  ~width=800.0,
  ~heightPx=600.0,
  (),
)
let unprojected = Perspective.unprojectAt(
  projected.sx,
  projected.sy,
  Model.defaultView,
  0.0,
  ~width=800.0,
  ~heightPx=600.0,
)
check(
  "perspective projection round trip",
  switch unprojected {
  | Some(point) => Math.abs(point.x -. 150.0) < 0.000001 && Math.abs(point.y -. 300.0) < 0.000001
  | None => false
  },
)
let farLeft = Perspective.project(
  ~x=-500.0,
  ~y=-400.0,
  ~view=Model.defaultView,
  ~width=800.0,
  ~heightPx=600.0,
  (),
)
let farRight = Perspective.project(
  ~x=500.0,
  ~y=-400.0,
  ~view=Model.defaultView,
  ~width=800.0,
  ~heightPx=600.0,
  (),
)
let nearLeft = Perspective.project(
  ~x=-500.0,
  ~y=400.0,
  ~view=Model.defaultView,
  ~width=800.0,
  ~heightPx=600.0,
  (),
)
let nearRight = Perspective.project(
  ~x=500.0,
  ~y=400.0,
  ~view=Model.defaultView,
  ~width=800.0,
  ~heightPx=600.0,
  (),
)
check(
  "perspective grid converges with depth",
  farRight.sx -. farLeft.sx < nearRight.sx -. nearLeft.sx,
)
let groundPoint = Perspective.project(
  ~x=0.0,
  ~y=0.0,
  ~view=Model.defaultView,
  ~width=800.0,
  ~heightPx=600.0,
  (),
)
let raisedPoint = Perspective.project(
  ~x=0.0,
  ~y=0.0,
  ~height=Perspective.pedestalHeight,
  ~view=Model.defaultView,
  ~width=800.0,
  ~heightPx=600.0,
  (),
)
check("pedestal height projects upward", raisedPoint.sy < groundPoint.sy)

let legacyStarter = {
  ...starter,
  shapes: starter.shapes->Array.map(shape =>
    shape.label == "hello"
      ? {...shape, x: -280.0, w: 180.0, fontSize: 24}
      : {...shape, x: 100.0, w: 180.0, fontSize: 24}
  ),
  edges: starter.edges->Array.map(edge => {...edge, directed: false, fontSize: 20}),
}
let upgradedStarter = Model.upgradeLegacyStarter(legacyStarter)
check(
  "legacy starter upgrades to perspective defaults",
  upgradedStarter.shapes->Array.some(shape =>
    shape.label == "hello" && shape.x == -380.0 && shape.w == 150.0 && shape.fontSize == 48
  ) && upgradedStarter.edges->Array.every(edge => edge.directed && edge.fontSize == 48),
)

let shared = starter->Share.encodeBoard->Share.decodeBoard
check(
  "share round trip",
  switch shared {
  | Some(board) => board.shapes->Array.length == 2 && board.edges->Array.length == 1
  | None => false
  },
)
let bentShared = bent->Share.encodeBoard->Share.decodeBoard
check(
  "share round trip preserves bends",
  switch bentShared {
  | Some(board) =>
    switch board.edges->Array.get(0) {
    | Some(edge) => edge.cx == Some(12.0) && edge.cy == Some(24.0)
    | None => false
    }
  | None => false
  },
)
check("invalid share rejected", "not-a-board"->Share.decodeBoard == None)

let generated = `{"name":"Service Flow","layers":[{"name":"Ground","color":"#38bdf8"}],"nodes":[{"id":"api","label":"API","kind":"rect","icon":"","color":"#0f2740","layer":0},{"id":"db","label":"Database","kind":"circle","icon":"","color":"#14532d","layer":0}],"edges":[{"from":"api","to":"db","label":"queries","directed":true}]}`
check(
  "generated graph import",
  switch Ai.boardFromGeneratedGraphJson(generated) {
  | Some(board) =>
    board.name == "Service Flow" &&
    board.shapes->Array.length == 2 &&
    board.edges->Array.length == 1
  | None => false
  },
)

if failures.contents > 0 {
  Console.error(failures.contents->Int.toString ++ " ReScript tests failed")
  assert(false)
} else {
  Console.log("All ReScript model tests passed.")
}
