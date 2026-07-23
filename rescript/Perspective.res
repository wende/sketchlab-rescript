open Model

@val @scope("window")
external innerWidth: float = "innerWidth"

@val @scope("window")
external innerHeight: float = "innerHeight"

type projected = {
  sx: float,
  sy: float,
  scale: float,
  depth: float,
  ok: bool,
}

type worldPoint = {
  x: float,
  y: float,
}

type bounds = {
  minX: float,
  minY: float,
  maxX: float,
  maxY: float,
}

let defaultDistance = 1200.0
let focalLength = 1200.0
let pitch = 3.141592653589793 /. 3.0
let pitchSin = Math.sin(pitch)
let pitchCos = Math.cos(pitch)
let pedestalHeight = 38.0
let arrowHeight = 9.0
let elevation = (layer: int, view: view): float => layer->Int.toFloat *. view.floorStep

let project = (
  ~x,
  ~y,
  ~height=0.0,
  ~view: view,
  ~width=innerWidth,
  ~heightPx=innerHeight,
  (),
): projected => {
  let distance = defaultDistance /. view.zoom
  let relativeX = x -. view.focusX
  let relativeDepth = view.focusY -. y
  let depth = distance +. relativeDepth *. pitchCos -. height *. pitchSin
  if depth <= 0.001 {
    {sx: 0.0, sy: 0.0, scale: 0.0, depth, ok: false}
  } else {
    let scale = focalLength /. depth
    {
      sx: width /. 2.0 +. view.panX +. relativeX *. scale,
      sy: heightPx /. 2.0 +. view.panY -.
        (relativeDepth *. pitchSin +. height *. pitchCos) *. scale,
      scale,
      depth,
      ok: true,
    }
  }
}

let unprojectAt = (
  screenX,
  screenY,
  view: view,
  height: float,
  ~width=innerWidth,
  ~heightPx=innerHeight,
): option<worldPoint> => {
  let distance = defaultDistance /. view.zoom
  let px = screenX -. (width /. 2.0 +. view.panX)
  let py = heightPx /. 2.0 +. view.panY -. screenY
  let denominator = focalLength *. pitchSin -. py *. pitchCos
  if denominator <= 0.000000001 {
    None
  } else {
    let relativeDepth =
      (py *. distance -. height *. (focalLength *. pitchCos +. py *. pitchSin)) /.
      denominator
    let depth = distance +. relativeDepth *. pitchCos -. height *. pitchSin
    if depth <= 0.001 {
      None
    } else {
      Some({
        x: view.focusX +. px *. depth /. focalLength,
        y: view.focusY -. relativeDepth,
      })
    }
  }
}

let unproject = (screenX, screenY, view: view): option<worldPoint> =>
  unprojectAt(screenX, screenY, view, 0.0)

let contentBounds = (board: board): option<bounds> => {
  if board.shapes->Array.length == 0 {
    None
  } else {
    Some(
      board.shapes->Array.reduce(
        {minX: 1000000000.0, minY: 1000000000.0, maxX: -.1000000000.0, maxY: -.1000000000.0},
        (acc, shape) => {
          minX: Math.min(acc.minX, shape.x),
          minY: Math.min(acc.minY, shape.y),
          maxX: Math.max(acc.maxX, shape.x +. shape.w),
          maxY: Math.max(acc.maxY, shape.y +. shape.h),
        },
      ),
    )
  }
}

let boardBounds = (board: board): bounds =>
  switch contentBounds(board) {
  | None => {minX: -900.0, minY: -680.0, maxX: 900.0, maxY: 680.0}
  | Some(content) =>
    let contentWidth = content.maxX -. content.minX
    let contentHeight = content.maxY -. content.minY
    let marginX = Math.max(520.0, contentWidth *. 0.45)
    let marginY = Math.max(420.0, contentHeight *. 0.45)
    {
      minX: content.minX -. marginX,
      minY: content.minY -. marginY,
      maxX: content.maxX +. marginX,
      maxY: content.maxY +. marginY,
    }
  }

let fitView = (board: board): view =>
  switch contentBounds(board) {
  | None => defaultView
  | Some(content) =>
    let contentWidth = Math.max(1.0, content.maxX -. content.minX)
    let contentHeight = Math.max(1.0, content.maxY -. content.minY)
    let highestLayer = board.shapes->Array.reduce(0, (highest, shape) =>
      Math.max(highest->Int.toFloat, shape.layer->Int.toFloat)->Float.toInt
    )
    let effectiveHeight = contentHeight +. highestLayer->Int.toFloat *. defaultView.floorStep *. 0.7
    let zoom =
      Math.min(innerWidth /. contentWidth, innerHeight /. effectiveHeight) *.
      0.5
      ->BoardOps.clampZoom
    {
      ...defaultView,
      zoom,
      focusX: (content.minX +. content.maxX) /. 2.0,
      focusY: (content.minY +. content.maxY) /. 2.0 -. contentHeight *. 0.2,
    }
  }
