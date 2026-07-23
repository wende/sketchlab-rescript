open Model

external styleFromDict: dict<string> => ReactDOMStyle.t = "%identity"

@val @scope("window")
external innerWidth: float = "innerWidth"

@val @scope("window")
external innerHeight: float = "innerHeight"

@val @return(nullable)
external prompt: (string, string) => option<string> = "prompt"

type gesture =
  | Idle
  | MovingShape(string, float, float, float, float)
  | Panning(float, float, float, float)
  | Drawing(float, float, float, float)
  | Resizing(string, float, float, float, float, float, float)
  | BendingEdge(string, float, float, float, float)

type dataTransfer
type fileList
type file
type fileReader

@get
external dataTransfer: 'event => dataTransfer = "dataTransfer"

@get
external files: dataTransfer => fileList = "files"

@get
external fileType: file => string = "type"

@send @return(nullable)
external fileAt: (fileList, int) => option<file> = "item"

@get
external dragClientX: 'event => int = "clientX"

@get
external dragClientY: 'event => int = "clientY"

@get
external ctrlKey: 'event => bool = "ctrlKey"

@get
external metaKey: 'event => bool = "metaKey"

@new
external makeFileReader: unit => fileReader = "FileReader"

@set
external setOnLoad: (fileReader, unit => unit) => unit = "onload"

@get @return(nullable)
external readerResult: fileReader => option<string> = "result"

@send
external readAsDataUrl: (fileReader, file) => unit = "readAsDataURL"

let screenToWorldAt = (clientX, clientY, view: view, height) =>
  switch Perspective.unprojectAt(clientX, clientY, view, height) {
  | Some(point) => (point.x, point.y)
  | None => (view.focusX, view.focusY)
  }

let screenToWorld = (clientX, clientY, view: view) =>
  screenToWorldAt(clientX, clientY, view, 0.0)

let shapeCenter = (shape: shape) => (
  shape.x +. shape.w /. 2.0,
  shape.y +. shape.h /. 2.0,
)

let edgeControlPoint = (board: board, edge: edge): option<(float, float)> =>
  switch (BoardOps.findShape(board, edge.from), BoardOps.findShape(board, edge.to_)) {
  | (Some(from), Some(to_)) =>
    let (x1, y1) = shapeCenter(from)
    let (x2, y2) = shapeCenter(to_)
    Some((
      switch edge.cx {
      | Some(value) => value
      | None => (x1 +. x2) /. 2.0
      },
      switch edge.cy {
      | Some(value) => value
      | None => (y1 +. y2) /. 2.0
      },
    ))
  | _ => None
  }

let shapeIsVisible = (board: board, shape: shape): bool =>
  switch board.layers->Array.get(shape.layer) {
  | Some(layer) => !layer.hidden
  | None => true
  }

let number = Float.toString

let pointString = (point: Perspective.projected) =>
  point.sx->number ++ "," ++ point.sy->number

let pointsString = (points: array<Perspective.projected>) =>
  points->Array.map(pointString)->Array.join(" ")

let pathFromPoints = (points: array<Perspective.projected>) =>
  points->Array.length == 0
    ? ""
    : "M " ++
      points
      ->Array.map(point => point.sx->number ++ " " ++ point.sy->number)
      ->Array.join(" L ")

let quad = (~x0, ~y0, ~x1, ~y1, ~height, ~view) => [
  Perspective.project(~x=x0, ~y=y0, ~height, ~view, ()),
  Perspective.project(~x=x1, ~y=y0, ~height, ~view, ()),
  Perspective.project(~x=x1, ~y=y1, ~height, ~view, ()),
  Perspective.project(~x=x0, ~y=y1, ~height, ~view, ()),
]

let ring = (~cx, ~cy, ~radius, ~height, ~view) =>
  Belt.Array.makeBy(30, index => {
    let angle = index->Int.toFloat /. 30.0 *. 3.141592653589793 *. 2.0
    Perspective.project(
      ~x=cx +. Math.cos(angle) *. radius,
      ~y=cy +. Math.sin(angle) *. radius,
      ~height,
      ~view,
      (),
    )
  })

let shapeOutline = (shape: shape, height, view) => {
  if shape.kind == "circle" || shape.kind == "icon" {
    ring(
      ~cx=shape.x +. shape.w /. 2.0,
      ~cy=shape.y +. shape.h /. 2.0,
      ~radius=Math.min(shape.w, shape.h) /. 2.0,
      ~height,
      ~view,
    )
  } else {
    quad(
      ~x0=shape.x,
      ~y0=shape.y,
      ~x1=shape.x +. shape.w,
      ~y1=shape.y +. shape.h,
      ~height,
      ~view,
    )
  }
}

let sideFacesPath = (
  top: array<Perspective.projected>,
  bottom: array<Perspective.projected>,
) => {
  let count = Math.min(top->Array.length->Int.toFloat, bottom->Array.length->Int.toFloat)->Float.toInt
  Belt.Array.makeBy(count, index => {
    let next = (index + 1) % count
    let a = top->Array.getUnsafe(index)
    let b = top->Array.getUnsafe(next)
    let c = bottom->Array.getUnsafe(next)
    let d = bottom->Array.getUnsafe(index)
    "M " ++
    a.sx->number ++
    " " ++
    a.sy->number ++
    " L " ++
    b.sx->number ++
    " " ++
    b.sy->number ++
    " L " ++
    c.sx->number ++
    " " ++
    c.sy->number ++
    " L " ++
    d.sx->number ++
    " " ++
    d.sy->number ++
    " Z"
  })->Array.join(" ")
}

let colorMix = (color, percent, target) =>
  color == noFill
    ? (target == "black" ? "#08131f" : "#94a3b8")
    : "color-mix(in srgb, " ++ color ++ " " ++ percent->Int.toString ++ "%, " ++ target ++ ")"

let boundaryPoint = (shape: shape, targetX, targetY) => {
  let (centerX, centerY) = shapeCenter(shape)
  let dx = targetX -. centerX
  let dy = targetY -. centerY
  if dx == 0.0 && dy == 0.0 {
    (centerX, centerY)
  } else if shape.kind == "circle" || shape.kind == "icon" {
    let radius = Math.min(shape.w, shape.h) /. 2.0
    let length = Math.hypot(dx, dy)
    (centerX +. dx /. length *. radius, centerY +. dy /. length *. radius)
  } else {
    let tx = dx == 0.0 ? 1000000000.0 : shape.w /. 2.0 /. Math.abs(dx)
    let ty = dy == 0.0 ? 1000000000.0 : shape.h /. 2.0 /. Math.abs(dy)
    let amount = Math.min(tx, ty)
    (centerX +. dx *. amount, centerY +. dy *. amount)
  }
}

let shapeClass = (shape: shape, selected: bool, pending: bool) => {
  let kind = "diagram-shape diagram-shape--" ++ shape.kind
  let selectedClass = selected ? " is-selected" : ""
  let pendingClass = pending ? " is-edge-source" : ""
  kind ++ selectedClass ++ pendingClass
}

module ShapeView = {
  @react.component
  let make = (
    ~shape: shape,
    ~view: view,
    ~selected,
    ~pending,
    ~onPointerDown,
    ~onDoubleClick,
    ~onResizePointerDown,
  ) => {
    let base = Perspective.elevation(shape.layer, view)
    let topHeight = base +. Perspective.pedestalHeight
    let bottom = shapeOutline(shape, base, view)
    let top = shapeOutline(shape, topHeight, view)
    let center = Perspective.project(
      ~x=shape.x +. shape.w /. 2.0,
      ~y=shape.y +. shape.h /. 2.0,
      ~height=topHeight,
      ~view,
      (),
    )
    let inset = Math.min(shape.w, shape.h) *. 0.12
    let face =
      if shape.kind == "circle" || shape.kind == "icon" {
        ring(
          ~cx=shape.x +. shape.w /. 2.0,
          ~cy=shape.y +. shape.h /. 2.0,
          ~radius=Math.max(3.0, Math.min(shape.w, shape.h) /. 2.0 -. inset),
          ~height=topHeight,
          ~view,
        )
      } else {
        quad(
          ~x0=shape.x +. inset,
          ~y0=shape.y +. inset,
          ~x1=shape.x +. shape.w -. inset,
          ~y1=shape.y +. shape.h -. inset,
          ~height=topHeight,
          ~view,
        )
      }
    let labelWidth =
      Math.max(
        60.0,
        shape.label->String.length->Int.toFloat *. shape.fontSize->Int.toFloat *. 0.58 +. 22.0,
      )
    let labelHeight = shape.fontSize->Int.toFloat *. 1.3 +. 10.0
    let labelTop = shape.y +. shape.h +. 10.0
    let labelPlate =
      quad(
        ~x0=shape.x +. shape.w /. 2.0 -. labelWidth /. 2.0,
        ~y0=labelTop,
        ~x1=shape.x +. shape.w /. 2.0 +. labelWidth /. 2.0,
        ~y1=labelTop +. labelHeight,
        ~height=base,
        ~view,
      )->Array.map(point => {...point, sy: point.sy +. 8.0})
    let projectedLabelPoint = Perspective.project(
      ~x=shape.x +. shape.w /. 2.0,
      ~y=labelTop +. labelHeight /. 2.0,
      ~height=base,
      ~view,
      (),
    )
    let labelPoint = {...projectedLabelPoint, sy: projectedLabelPoint.sy +. 8.0}
    let resizePoint =
      top->Array.getUnsafe(shape.kind == "circle" || shape.kind == "icon" ? 4 : 2)

    <g
      className={shapeClass(shape, selected, pending)}
      onPointerDown
      onDoubleClick
      role="button"
      ariaLabel={shape.label == "" ? shape.kind : shape.label}
    >
      {shape.kind == "text"
        ? React.null
        : <g>
            <polygon className="diagram-shape__shadow" points={pointsString(bottom)} />
            <path
              className="diagram-shape__walls"
              d={sideFacesPath(top, bottom)}
              fill={colorMix(shape.fill, 34, "black")}
            />
            <polygon
              className="diagram-shape__top"
              points={pointsString(top)}
              fill={shape.fill == noFill ? "rgba(7, 21, 34, 0.35)" : shape.fill}
              stroke={selected ? "#a5f3fc" : colorMix(shape.fill, 46, "white")}
            />
            <polygon
              className="diagram-shape__face"
              points={pointsString(face)}
              fill={shape.fill == noFill
                ? "rgba(15, 39, 64, 0.28)"
                : colorMix(shape.fill, 84, "white")}
            />
          </g>}
      {shape.kind == "image"
        ? switch shape.src {
          | Some(src) =>
            <image
              href={src}
              x={(center.sx -. shape.w *. center.scale *. 0.38)->number}
              y={(center.sy -. shape.h *. center.scale *. 0.3)->number}
              width={(shape.w *. center.scale *. 0.76)->number}
              height={(shape.h *. center.scale *. 0.6)->number}
              preserveAspectRatio="xMidYMid meet"
            />
          | None => React.null
          }
        : React.null}
      {shape.kind == "icon"
        ? <text
            x={center.sx->number}
            y={center.sy->number}
            className="diagram-icon"
            textAnchor="middle"
            dominantBaseline="middle"
            fontSize={(Math.max(12.0, 56.0 *. center.scale))->number}
          >
            {React.string(
              switch shape.icon {
              | Some(icon) => icon
              | None => "◇"
              },
            )}
          </text>
        : React.null}
      {shape.kind == "code"
        ? <text
            x={(center.sx -. shape.w *. center.scale *. 0.38)->number}
            y={(center.sy -. shape.h *. center.scale *. 0.12)->number}
            className="diagram-code"
            fontSize={(shape.fontSize->Int.toFloat *. center.scale *. 0.62)->number}
          >
            {React.string(shape.label == "" ? "code" : shape.label)}
          </text>
        : shape.kind == "text"
        ? <text
            x={center.sx->number}
            y={center.sy->number}
            className="diagram-label diagram-label--free"
            textAnchor="middle"
            dominantBaseline="middle"
            fontSize={(shape.fontSize->Int.toFloat *. center.scale)->number}
          >
            {React.string(shape.label)}
          </text>
        : shape.label == ""
        ? React.null
        : <g className="diagram-nameplate">
            <polygon points={pointsString(labelPlate)} />
            <text
              x={labelPoint.sx->number}
              y={(labelPoint.sy +. 2.0)->number}
              textAnchor="middle"
              dominantBaseline="middle"
              fontSize={(Math.max(
                10.0,
                shape.fontSize->Int.toFloat *. labelPoint.scale,
              ))->number}
            >
              {React.string(shape.label)}
            </text>
          </g>}
      {selected
        ? <g className="diagram-resize-handle" onPointerDown={onResizePointerDown}>
            <circle cx={resizePoint.sx->number} cy={resizePoint.sy->number} r="8" />
          </g>
        : React.null}
    </g>
  }
}

module EdgeView = {
  @react.component
  let make = (
    ~edge: edge,
    ~board: board,
    ~view: view,
    ~selected,
    ~onPointerDown,
    ~onDoubleClick,
    ~onBendPointerDown,
  ) =>
    switch (BoardOps.findShape(board, edge.from), BoardOps.findShape(board, edge.to_)) {
    | (Some(from), Some(to_)) if shapeIsVisible(board, from) && shapeIsVisible(board, to_) =>
      let (fromCenterX, fromCenterY) = shapeCenter(from)
      let (toCenterX, toCenterY) = shapeCenter(to_)
      let centerX = switch edge.cx {
      | Some(value) => value
      | None => (fromCenterX +. toCenterX) /. 2.0
      }
      let centerY = switch edge.cy {
      | Some(value) => value
      | None => (fromCenterY +. toCenterY) /. 2.0
      }
      let hasBend = edge.cx != None && edge.cy != None
      let (x1, y1) = boundaryPoint(
        from,
        hasBend ? centerX : toCenterX,
        hasBend ? centerY : toCenterY,
      )
      let (x2, y2) = boundaryPoint(
        to_,
        hasBend ? centerX : fromCenterX,
        hasBend ? centerY : fromCenterY,
      )
      let points = Belt.Array.makeBy(hasBend ? 25 : 2, index => {
        let t = hasBend ? index->Int.toFloat /. 24.0 : index->Int.toFloat
        let inverse = 1.0 -. t
        let worldX = hasBend
          ? inverse *. inverse *. x1 +. 2.0 *. inverse *. t *. centerX +. t *. t *. x2
          : x1 +. (x2 -. x1) *. t
        let worldY = hasBend
          ? inverse *. inverse *. y1 +. 2.0 *. inverse *. t *. centerY +. t *. t *. y2
          : y1 +. (y2 -. y1) *. t
        let height =
          Perspective.elevation(from.layer, view) +. Perspective.arrowHeight +.
          (Perspective.elevation(to_.layer, view) -. Perspective.elevation(from.layer, view)) *. t
        Perspective.project(~x=worldX, ~y=worldY, ~height, ~view, ())
      })
      let path = pathFromPoints(points)
      let labelPoint = Perspective.project(
        ~x=centerX,
        ~y=centerY,
        ~height=(Perspective.elevation(from.layer, view) +. Perspective.elevation(to_.layer, view)) /. 2.0 +.
          Perspective.arrowHeight,
        ~view,
        (),
      )
      <g
        className={selected ? "diagram-edge is-selected" : "diagram-edge"}
        onPointerDown
        onDoubleClick
      >
        <path className="diagram-edge__glow" d={path} />
        <path
          className="diagram-edge__core"
          d={path}
          markerEnd={edge.directed ? "url(#arrowhead)" : ""}
        />
        {edge.directed ? <path className="diagram-edge__flow" d={path} /> : React.null}
        <path className="diagram-edge__hit" d={path} />
        {edge.label == ""
          ? React.null
          : <g className="diagram-edge__label">
              <rect
                x={(labelPoint.sx -.
                  edge.label->String.length->Int.toFloat *.
                  edge.fontSize->Int.toFloat *.
                  labelPoint.scale *.
                  0.3 -.
                  9.0)->number}
                y={(labelPoint.sy -. edge.fontSize->Int.toFloat *. labelPoint.scale *. 0.7)->number}
                width={(edge.label->String.length->Int.toFloat *.
                  edge.fontSize->Int.toFloat *.
                  labelPoint.scale *.
                  0.6 +.
                  18.0)->number}
                height={(edge.fontSize->Int.toFloat *. labelPoint.scale +. 10.0)->number}
                rx="4"
              />
              <text
                x={labelPoint.sx->number}
                y={labelPoint.sy->number}
                textAnchor="middle"
                dominantBaseline="middle"
                fontSize={(edge.fontSize->Int.toFloat *. labelPoint.scale)->number}
              >
                {React.string(edge.label)}
              </text>
            </g>}
        {selected
          ? <circle
              className="diagram-edge__bend"
              cx={labelPoint.sx->number}
              cy={labelPoint.sy->number}
              r="8"
              onPointerDown={onBendPointerDown}
            />
          : React.null}
      </g>
    | _ => React.null
    }
}

@react.component
let make = (
  ~board: board,
  ~view: view,
  ~tool: string,
  ~spacePressed: bool,
  ~fill: string,
  ~fontSize: int,
  ~selection: selection,
  ~onBoard: board => unit,
  ~onView: view => unit,
  ~onSelection: selection => unit,
  ~onTool: string => unit,
) => {
  let (gesture, setGesture) = React.useState(() => Idle)
  let (pendingEdgeFrom, setPendingEdgeFrom) = React.useState(() => None)

  let selectedShapeId = switch selection {
  | ShapeSelection(id) => Some(id)
  | _ => None
  }
  let selectedEdgeId = switch selection {
  | EdgeSelection(id) => Some(id)
  | _ => None
  }

  let startBackground = event => {
    let clientX = event->ReactEvent.Pointer.clientX->Int.toFloat
    let clientY = event->ReactEvent.Pointer.clientY->Int.toFloat
    let (worldX, worldY) = screenToWorldAt(
      clientX,
      clientY,
      view,
      Perspective.elevation(view.activeLayer, view),
    )
    if spacePressed {
      setGesture(_ => Panning(clientX, clientY, view.panX, view.panY))
    } else {
      onSelection(NoSelection)
      switch tool {
      | "rect" | "circle" => setGesture(_ => Drawing(worldX, worldY, worldX, worldY))
      | "text" | "code" =>
        switch prompt(tool == "code" ? "Enter code" : "Enter text", "") {
        | Some(label) =>
          let shape = makeShape(
            ~kind=tool,
            ~x=worldX -. 90.0,
            ~y=worldY -. 45.0,
            ~w=180.0,
            ~h=tool == "text" ? 60.0 : 120.0,
            ~label,
            ~fill=tool == "text" ? noFill : fill,
            ~layer=view.activeLayer,
            ~fontSize,
            (),
          )
          onBoard(BoardOps.addShape(board, shape))
          onSelection(ShapeSelection(shape.id))
          onTool("select")
        | None => ()
        }
      | "hand" | "select" => setGesture(_ => Panning(clientX, clientY, view.panX, view.panY))
      | _ => ()
      }
    }
  }

  let startShape = (event, shape: shape) => {
    event->ReactEvent.Pointer.stopPropagation
    let clientX = event->ReactEvent.Pointer.clientX->Int.toFloat
    let clientY = event->ReactEvent.Pointer.clientY->Int.toFloat
    let (worldX, worldY) = screenToWorldAt(
      clientX,
      clientY,
      view,
      Perspective.elevation(shape.layer, view),
    )
    if spacePressed {
      setGesture(_ => Panning(clientX, clientY, view.panX, view.panY))
    } else if tool == "line" || tool == "arrow" {
      switch pendingEdgeFrom {
      | None =>
        setPendingEdgeFrom(_ => Some(shape.id))
        onSelection(ShapeSelection(shape.id))
      | Some(from) =>
        if from != shape.id {
          onBoard(
            BoardOps.addEdge(
              board,
              makeEdge(~from, ~to_=shape.id, ~directed=tool == "arrow", ~fontSize, ()),
            ),
          )
        }
        setPendingEdgeFrom(_ => None)
        onTool("select")
      }
    } else {
      onSelection(ShapeSelection(shape.id))
      setGesture(_ => MovingShape(shape.id, worldX, worldY, shape.x, shape.y))
    }
  }

  let startResize = (event, shape: shape) => {
    event->ReactEvent.Pointer.stopPropagation
    let clientX = event->ReactEvent.Pointer.clientX->Int.toFloat
    let clientY = event->ReactEvent.Pointer.clientY->Int.toFloat
    let (worldX, worldY) = screenToWorldAt(
      clientX,
      clientY,
      view,
      Perspective.elevation(shape.layer, view),
    )
    setGesture(_ => Resizing(shape.id, worldX, worldY, shape.w, shape.h, shape.x, shape.y))
  }

  let startBend = (event, edge: edge) => {
    event->ReactEvent.Pointer.stopPropagation
    let clientX = event->ReactEvent.Pointer.clientX->Int.toFloat
    let clientY = event->ReactEvent.Pointer.clientY->Int.toFloat
    let bendHeight = switch (BoardOps.findShape(board, edge.from), BoardOps.findShape(board, edge.to_)) {
    | (Some(from), Some(to_)) =>
      (Perspective.elevation(from.layer, view) +. Perspective.elevation(to_.layer, view)) /. 2.0 +.
      Perspective.arrowHeight
    | _ => Perspective.elevation(view.activeLayer, view) +. Perspective.arrowHeight
    }
    let (worldX, worldY) = screenToWorldAt(clientX, clientY, view, bendHeight)
    switch edgeControlPoint(board, edge) {
    | Some((cx, cy)) =>
      onSelection(EdgeSelection(edge.id))
      setGesture(_ => BendingEdge(edge.id, worldX -. cx, worldY -. cy, cx, cy))
    | None => ()
    }
  }

  let movePointer = event => {
    let clientX = event->ReactEvent.Pointer.clientX->Int.toFloat
    let clientY = event->ReactEvent.Pointer.clientY->Int.toFloat
    switch gesture {
    | Idle => ()
    | Panning(startX, startY, panX, panY) =>
      onView({
        ...view,
        panX: panX +. clientX -. startX,
        panY: panY +. clientY -. startY,
      })
    | MovingShape(id, startX, startY, shapeX, shapeY) =>
      switch BoardOps.findShape(board, id) {
      | Some(current) =>
        let (worldX, worldY) = screenToWorldAt(
          clientX,
          clientY,
          view,
          Perspective.elevation(current.layer, view),
        )
        onBoard(
          BoardOps.updateShape(board, id, shape => {
            ...shape,
            x: shapeX +. worldX -. startX,
            y: shapeY +. worldY -. startY,
          }),
        )
      | None => ()
      }
    | Drawing(startX, startY, _endX, _endY) =>
      let (worldX, worldY) = screenToWorldAt(
        clientX,
        clientY,
        view,
        Perspective.elevation(view.activeLayer, view),
      )
      setGesture(_ => Drawing(startX, startY, worldX, worldY))
    | Resizing(id, startX, startY, startW, startH, _shapeX, _shapeY) =>
      switch BoardOps.findShape(board, id) {
      | Some(current) =>
        let (worldX, worldY) = screenToWorldAt(
          clientX,
          clientY,
          view,
          Perspective.elevation(current.layer, view),
        )
        onBoard(
          BoardOps.updateShape(board, id, shape => {
            let nextWidth = Math.max(36.0, startW +. worldX -. startX)
            let nextHeight = Math.max(28.0, startH +. worldY -. startY)
            let (width, height) = if shape.kind == "circle" {
              let size = Math.max(nextWidth, nextHeight)
              (size, size)
            } else if shape.kind == "image" {
              let ratio = startW /. startH
              (nextWidth, Math.max(28.0, nextWidth /. ratio))
            } else {
              (nextWidth, nextHeight)
            }
            {
              ...shape,
              w: width,
              h: height,
            }
          }),
        )
      | None => ()
      }
    | BendingEdge(id, offsetX, offsetY, _startCx, _startCy) =>
      let (worldX, worldY) = screenToWorldAt(
        clientX,
        clientY,
        view,
        Perspective.elevation(view.activeLayer, view) +. Perspective.arrowHeight,
      )
      onBoard(
        BoardOps.updateEdge(board, id, edge => {
          ...edge,
          cx: Some(worldX -. offsetX),
          cy: Some(worldY -. offsetY),
        }),
      )
    }
  }

  let finishPointer = _event => {
    switch gesture {
    | Drawing(startX, startY, endX, endY) =>
      let x = Math.min(startX, endX)
      let y = Math.min(startY, endY)
      let width = Math.max(36.0, Math.abs(endX -. startX))
      let height = Math.max(36.0, Math.abs(endY -. startY))
      let size = tool == "circle" ? Math.max(width, height) : 0.0
      let shape = makeShape(
        ~kind=tool,
        ~x,
        ~y,
        ~w=tool == "circle" ? size : width,
        ~h=tool == "circle" ? size : height,
        ~fill,
        ~layer=view.activeLayer,
        ~fontSize,
        (),
      )
      onBoard(BoardOps.addShape(board, shape))
      onSelection(ShapeSelection(shape.id))
      onTool("select")
    | _ => ()
    }
    setGesture(_ => Idle)
  }

  let editShape = (event, shape: shape) => {
    event->ReactEvent.Mouse.stopPropagation
    switch prompt("Edit label", shape.label) {
    | Some(label) => onBoard(BoardOps.updateShape(board, shape.id, current => {...current, label}))
    | None => ()
    }
  }

  let editEdge = (event, edge: edge) => {
    event->ReactEvent.Mouse.stopPropagation
    switch prompt("Edit connector label", edge.label) {
    | Some(label) =>
      onBoard(
        BoardOps.touch({
          ...board,
          edges: board.edges->Array.map(current =>
            current.id == edge.id ? {...current, label} : current
          ),
        }),
      )
    | None => ()
    }
  }

  let zoom = event => {
    event->ReactEvent.Wheel.preventDefault
    if event->ctrlKey || event->metaKey {
      let delta = event->ReactEvent.Wheel.deltaY
      let factor = delta > 0.0 ? 0.9 : 1.1
      onView({...view, zoom: BoardOps.clampZoom(view.zoom *. factor)})
    } else {
      onView({
        ...view,
        panX: view.panX -. event->ReactEvent.Wheel.deltaX,
        panY: view.panY -. event->ReactEvent.Wheel.deltaY,
      })
    }
  }

  let doubleClickBackground = event => {
    event->ReactEvent.Mouse.stopPropagation
    let clientX = event->ReactEvent.Mouse.clientX->Int.toFloat
    let clientY = event->ReactEvent.Mouse.clientY->Int.toFloat
    let (worldX, worldY) = screenToWorldAt(
      clientX,
      clientY,
      view,
      Perspective.elevation(view.activeLayer, view),
    )
    switch prompt("Enter text", "") {
    | Some(label) if label->String.trim != "" =>
      let shape = makeShape(
        ~kind="text",
        ~x=worldX -. 110.0,
        ~y=worldY -. 30.0,
        ~w=220.0,
        ~h=60.0,
        ~label,
        ~fill=noFill,
        ~layer=view.activeLayer,
        ~fontSize,
        (),
      )
      onBoard(BoardOps.addShape(board, shape))
      onSelection(ShapeSelection(shape.id))
      onTool("select")
    | _ => ()
    }
  }

  let dropImage = event => {
    event->ReactEvent.toSyntheticEvent->ReactEvent.Synthetic.preventDefault
    let clientX = event->dragClientX->Int.toFloat
    let clientY = event->dragClientY->Int.toFloat
    let (worldX, worldY) = screenToWorldAt(
      clientX,
      clientY,
      view,
      Perspective.elevation(view.activeLayer, view),
    )
    switch event->dataTransfer->files->fileAt(0) {
    | Some(file) if file->fileType->String.startsWith("image/") =>
      let reader = makeFileReader()
      reader->setOnLoad(() =>
        switch reader->readerResult {
        | Some(src) =>
          let shape = makeShape(
            ~kind="image",
            ~x=worldX -. 120.0,
            ~y=worldY -. 90.0,
            ~w=240.0,
            ~h=180.0,
            ~fill=noFill,
            ~layer=view.activeLayer,
            ~src,
            (),
          )
          onBoard(BoardOps.addShape(board, shape))
          onSelection(ShapeSelection(shape.id))
        | None => ()
        }
      )
      reader->readAsDataUrl(file)
    | _ => ()
    }
  }

  let floorBounds = Perspective.boardBounds(board)
  let fieldPoints = quad(
    ~x0=floorBounds.minX,
    ~y0=floorBounds.minY,
    ~x1=floorBounds.maxX,
    ~y1=floorBounds.maxY,
    ~height=0.0,
    ~view,
  )
  let gridStep = 48.0
  let firstGridX = Math.ceil(floorBounds.minX /. gridStep) *. gridStep
  let firstGridY = Math.ceil(floorBounds.minY /. gridStep) *. gridStep
  let gridXCount =
    Math.max(0.0, Math.floor((floorBounds.maxX -. firstGridX) /. gridStep) +. 1.0)
    ->Float.toInt
  let gridYCount =
    Math.max(0.0, Math.floor((floorBounds.maxY -. firstGridY) /. gridStep) +. 1.0)
    ->Float.toInt
  let gridXs = Belt.Array.makeBy(
    gridXCount,
    index => firstGridX +. index->Int.toFloat *. gridStep,
  )
  let gridYs = Belt.Array.makeBy(
    gridYCount,
    index => firstGridY +. index->Int.toFloat *. gridStep,
  )
  let isMajorGrid = value =>
    Math.abs(value /. 240.0 -. Math.round(value /. 240.0)) < 0.0001

  let draftElement = switch gesture {
  | Drawing(startX, startY, endX, endY) =>
    let x = Math.min(startX, endX)
    let y = Math.min(startY, endY)
    let width = Math.abs(endX -. startX)
    let height = Math.abs(endY -. startY)
    let elevation = Perspective.elevation(view.activeLayer, view) +. Perspective.pedestalHeight
    let draftPoints = if tool == "circle" {
      ring(
        ~cx=x +. width /. 2.0,
        ~cy=y +. height /. 2.0,
        ~radius=Math.max(width, height) /. 2.0,
        ~height=elevation,
        ~view,
      )
    } else {
      quad(~x0=x, ~y0=y, ~x1=x +. width, ~y1=y +. height, ~height=elevation, ~view)
    }
    <polygon
      className="diagram-draft"
      points={pointsString(draftPoints)}
    />
  | _ => React.null
  }

  <svg
    className={"diagram-canvas cursor--" ++ tool}
    onPointerDown={startBackground}
    onPointerMove={movePointer}
    onPointerUp={finishPointer}
    onPointerLeave={finishPointer}
    onDoubleClick={doubleClickBackground}
    onWheel={zoom}
    onDragOver={event => event->ReactEvent.toSyntheticEvent->ReactEvent.Synthetic.preventDefault}
    onDrop={dropImage}
    role="application"
    ariaLabel="Sketch Lab diagram canvas"
  >
    <defs>
      <filter id="cyan-glow" x="-50%" y="-50%" width="200%" height="200%">
        <feGaussianBlur stdDeviation="3" result="blur" />
        <feMerge>
          <feMergeNode in_="blur" />
          <feMergeNode in_="SourceGraphic" />
        </feMerge>
      </filter>
      <marker
        id="arrowhead"
        markerWidth="16"
        markerHeight="12"
        refX="14"
        refY="6"
        orient="auto"
        markerUnits="userSpaceOnUse"
      >
        <polygon points="0 0, 16 6, 0 12" />
      </marker>
    </defs>
    <polygon className="diagram-field" points={pointsString(fieldPoints)} />
    <g className="diagram-grid">
      {gridXs
      ->Array.map(value => {
        let far = Perspective.project(~x=value, ~y=floorBounds.minY, ~view, ())
        let near = Perspective.project(~x=value, ~y=floorBounds.maxY, ~view, ())
        <line
          key={"x-" ++ value->number}
          className={isMajorGrid(value) ? "diagram-grid-line is-major" : "diagram-grid-line"}
          x1={far.sx->number}
          y1={far.sy->number}
          x2={near.sx->number}
          y2={near.sy->number}
        />
      })
      ->React.array}
      {gridYs
      ->Array.map(value => {
        let left = Perspective.project(~x=floorBounds.minX, ~y=value, ~view, ())
        let right = Perspective.project(~x=floorBounds.maxX, ~y=value, ~view, ())
        <line
          key={"y-" ++ value->number}
          className={isMajorGrid(value) ? "diagram-grid-line is-major" : "diagram-grid-line"}
          x1={left.sx->number}
          y1={left.sy->number}
          x2={right.sx->number}
          y2={right.sy->number}
        />
      })
      ->React.array}
    </g>
    {board.layers
    ->Array.mapWithIndex((layer, index) => {
      let height = Perspective.elevation(index, view)
      let frame = quad(
        ~x0=floorBounds.minX,
        ~y0=floorBounds.minY,
        ~x1=floorBounds.maxX,
        ~y1=floorBounds.maxY,
        ~height,
        ~view,
      )
      let badge = Perspective.project(
        ~x=floorBounds.minX +. 32.0,
        ~y=floorBounds.minY +. 32.0,
        ~height,
        ~view,
        (),
      )
      layer.hidden
        ? React.null
        : <g
            key={layer.id}
            className={view.activeLayer == index
              ? "diagram-floor is-active"
              : "diagram-floor"}
            style={dict{"--floor-color": layer.color}->styleFromDict}
          >
            <polygon className="diagram-floor__plate" points={pointsString(frame)} />
            <polygon className="diagram-floor__glow" points={pointsString(frame)} />
            <polygon className="diagram-floor__core" points={pointsString(frame)} />
            {badge.sx < 0.0 || badge.sx > innerWidth
              ? React.null
              : <text
                  x={badge.sx->number}
                  y={badge.sy->number}
                  fill={layer.color}
                  fontSize={(Math.max(9.0, 15.0 *. badge.scale))->number}
                >
                  {React.string(layer.name)}
                </text>}
          </g>
    })
    ->React.array}
    {board.edges
    ->Array.map(edge =>
      <EdgeView
        key={edge.id}
        edge
        board
        view
        selected={selectedEdgeId == Some(edge.id)}
        onPointerDown={event => {
          event->ReactEvent.Pointer.stopPropagation
          onSelection(EdgeSelection(edge.id))
        }}
        onDoubleClick={event => editEdge(event, edge)}
        onBendPointerDown={event => startBend(event, edge)}
      />
    )
    ->React.array}
    {board.shapes
    ->Array.filter(shape => shapeIsVisible(board, shape))
    ->Array.toSorted((a, b) => {
      let aDepth = Perspective.project(
        ~x=a.x +. a.w /. 2.0,
        ~y=a.y +. a.h /. 2.0,
        ~height=Perspective.elevation(a.layer, view),
        ~view,
        (),
      ).depth
      let bDepth = Perspective.project(
        ~x=b.x +. b.w /. 2.0,
        ~y=b.y +. b.h /. 2.0,
        ~height=Perspective.elevation(b.layer, view),
        ~view,
        (),
      ).depth
      aDepth > bDepth ? -1.0 : aDepth < bDepth ? 1.0 : 0.0
    })
    ->Array.map(shape =>
      <ShapeView
        key={shape.id}
        shape
        view
        selected={selectedShapeId == Some(shape.id)}
        pending={pendingEdgeFrom == Some(shape.id)}
        onPointerDown={event => startShape(event, shape)}
        onDoubleClick={event => editShape(event, shape)}
        onResizePointerDown={event => startResize(event, shape)}
      />
    )
    ->React.array}
    {draftElement}
  </svg>
}
