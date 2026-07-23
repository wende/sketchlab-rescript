type shape = {
  id: string,
  kind: string,
  x: float,
  y: float,
  w: float,
  h: float,
  label: string,
  fill: string,
  layer: int,
  fontSize: int,
  src: option<string>,
  icon: option<string>,
}

type edge = {
  id: string,
  from: string,
  to_: string,
  label: string,
  directed: bool,
  fontSize: int,
  cx: option<float>,
  cy: option<float>,
}

type layer = {
  id: string,
  name: string,
  hidden: bool,
  color: string,
}

type board = {
  id: string,
  name: string,
  shapes: array<shape>,
  edges: array<edge>,
  layers: array<layer>,
  createdAt: float,
  updatedAt: float,
}

type selection =
  | NoSelection
  | ShapeSelection(string)
  | EdgeSelection(string)

type view = {
  zoom: float,
  panX: float,
  panY: float,
  focusX: float,
  focusY: float,
  floorStep: float,
  activeLayer: int,
}

type boardMeta = {
  id: string,
  name: string,
  updatedAt: float,
  shapeCount: int,
}

let noFill = "transparent"

let defaultView = {
  zoom: 1.0,
  panX: 0.0,
  panY: 0.0,
  focusX: 0.0,
  focusY: 0.0,
  floorStep: 220.0,
  activeLayer: 0,
}

let groundLayer = {
  id: "ground",
  name: "Ground",
  hidden: false,
  color: "#38bdf8",
}

let uid = () => {
  let time = Date.now()->Float.toString
  let random = Math.random()->Float.toString
  time ++ "-" ++ random
}

let makeShape = (
  ~kind,
  ~x,
  ~y,
  ~w=150.0,
  ~h=110.0,
  ~label="",
  ~fill="#0f2740",
  ~layer=0,
  ~fontSize=48,
  ~src=?,
  ~icon=?,
  (),
): shape => {
  id: uid(),
  kind,
  x,
  y,
  w,
  h,
  label,
  fill,
  layer,
  fontSize,
  src,
  icon,
}

let makeEdge = (~from, ~to_, ~directed=false, ~label="", ~fontSize=48, ()): edge => {
  id: uid(),
  from,
  to_,
  label,
  directed,
  fontSize,
  cx: None,
  cy: None,
}

let emptyBoard = (~name="Untitled board", ()): board => {
  let now = Date.now()
  {
    id: uid(),
    name,
    shapes: [],
    edges: [],
    layers: [groundLayer],
    createdAt: now,
    updatedAt: now,
  }
}

let starterBoard = (): board => {
  let left = makeShape(~kind="rect", ~x=-380.0, ~y=-55.0, ~label="hello", ())
  let right = makeShape(~kind="rect", ~x=230.0, ~y=-55.0, ~label="world", ())
  let now = Date.now()
  {
    id: uid(),
    name: "Untitled board",
    shapes: [left, right],
    edges: [makeEdge(~from=left.id, ~to_=right.id, ~directed=true, ())],
    layers: [groundLayer],
    createdAt: now,
    updatedAt: now,
  }
}

let upgradeLegacyStarter = (board: board): board => {
  if board.shapes->Array.length == 2 && board.edges->Array.length == 1 {
    let hello = board.shapes->Array.find(shape =>
      shape.label == "hello" &&
      shape.kind == "rect" &&
      shape.x == -280.0 &&
      shape.y == -55.0 &&
      shape.w == 180.0 &&
      shape.h == 110.0
    )
    let world = board.shapes->Array.find(shape =>
      shape.label == "world" &&
      shape.kind == "rect" &&
      shape.x == 100.0 &&
      shape.y == -55.0 &&
      shape.w == 180.0 &&
      shape.h == 110.0
    )
    switch (hello, world) {
    | (Some(helloShape), Some(worldShape)) =>
      {
        ...board,
        shapes: board.shapes->Array.map(shape =>
          shape.id == helloShape.id
            ? {...shape, x: -380.0, w: 150.0, fontSize: 48}
            : shape.id == worldShape.id
            ? {...shape, x: 230.0, w: 150.0, fontSize: 48}
            : shape
        ),
        edges: board.edges->Array.map(edge => {...edge, directed: true, fontSize: 48}),
      }
    | _ => board
    }
  } else {
    board
  }
}
