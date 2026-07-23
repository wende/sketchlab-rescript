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
  ~w=180.0,
  ~h=110.0,
  ~label="",
  ~fill="#0f2740",
  ~layer=0,
  ~fontSize=24,
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

let makeEdge = (~from, ~to_, ~directed=false, ~label="", ~fontSize=20, ()): edge => {
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
  let left = makeShape(~kind="rect", ~x=-280.0, ~y=-55.0, ~label="hello", ())
  let right = makeShape(~kind="rect", ~x=100.0, ~y=-55.0, ~label="world", ())
  let now = Date.now()
  {
    id: uid(),
    name: "Untitled board",
    shapes: [left, right],
    edges: [makeEdge(~from=left.id, ~to_=right.id, ())],
    layers: [groundLayer],
    createdAt: now,
    updatedAt: now,
  }
}
