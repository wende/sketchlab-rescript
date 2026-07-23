open Model

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

let screenToWorld = (clientX, clientY, view: view) => (
  (clientX -. innerWidth /. 2.0 -. view.panX) /. view.zoom,
  (clientY -. innerHeight /. 2.0 -. view.panY) /. view.zoom,
)

let shapeCenter = (shape: shape) => (
  shape.x +. shape.w /. 2.0,
  shape.y +. shape.h /. 2.0 -. shape.layer->Int.toFloat *. 26.0,
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
    ~selected,
    ~pending,
    ~onPointerDown,
    ~onDoubleClick,
    ~onResizePointerDown,
  ) => {
    let layerOffset = shape.layer->Int.toFloat *. -26.0
    let labelY = shape.y +. shape.h +. 32.0 +. layerOffset
    let common =
      <rect
        x={shape.x->number}
        y={(shape.y +. layerOffset)->number}
        width={shape.w->number}
        height={shape.h->number}
        rx={(shape.kind == "circle" ? shape.w /. 2.0 : 12.0)->number}
        fill={shape.fill == noFill ? "transparent" : shape.fill}
        stroke={selected ? "#7dd3fc" : "#60758d"}
        strokeWidth={(selected ? 3.0 : 2.0)->number}
      />

    <g
      className={shapeClass(shape, selected, pending)}
      onPointerDown
      onDoubleClick
      role="button"
      ariaLabel={shape.label == "" ? shape.kind : shape.label}
    >
      {switch shape.kind {
      | "image" =>
        switch shape.src {
        | Some(src) =>
          <image
            href={src}
            x={shape.x->number}
            y={(shape.y +. layerOffset)->number}
            width={shape.w->number}
            height={shape.h->number}
            preserveAspectRatio="xMidYMid meet"
          />
        | None => common
        }
      | "text" => React.null
      | _ => common
      }}
      {shape.kind == "icon"
        ? <text
            x={(shape.x +. shape.w /. 2.0)->number}
            y={(shape.y +. layerOffset +. shape.h /. 2.0)->number}
            className="diagram-icon"
            textAnchor="middle"
            dominantBaseline="middle"
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
            x={(shape.x +. 14.0)->number}
            y={(shape.y +. layerOffset +. 34.0)->number}
            className="diagram-code"
            fontSize={shape.fontSize->Int.toString}
          >
            {React.string(shape.label == "" ? "code" : shape.label)}
          </text>
        : <text
            x={(shape.x +. shape.w /. 2.0)->number}
            y={(shape.kind == "text" ? shape.y +. layerOffset +. shape.h /. 2.0 : labelY)->number}
            className="diagram-label"
            textAnchor="middle"
            dominantBaseline="middle"
            fontSize={shape.fontSize->Int.toString}
          >
            {React.string(shape.label)}
          </text>}
      {selected
        ? <g className="diagram-resize-handle" onPointerDown={onResizePointerDown}>
            <circle
              cx={(shape.x +. shape.w)->number}
              cy={(shape.y +. layerOffset +. shape.h)->number}
              r="8"
            />
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
    ~selected,
    ~onPointerDown,
    ~onDoubleClick,
    ~onBendPointerDown,
  ) =>
    switch (BoardOps.findShape(board, edge.from), BoardOps.findShape(board, edge.to_)) {
    | (Some(from), Some(to_)) if shapeIsVisible(board, from) && shapeIsVisible(board, to_) =>
      let (x1, y1) = shapeCenter(from)
      let (x2, y2) = shapeCenter(to_)
      let centerX = switch edge.cx {
      | Some(value) => value
      | None => (x1 +. x2) /. 2.0
      }
      let centerY = switch edge.cy {
      | Some(value) => value
      | None => (y1 +. y2) /. 2.0
      }
      let path =
        "M " ++
        x1->number ++
        " " ++
        y1->number ++
        " Q " ++
        centerX->number ++
        " " ++
        centerY->number ++
        " " ++
        x2->number ++
        " " ++
        y2->number
      <g
        className={selected ? "diagram-edge is-selected" : "diagram-edge"}
        onPointerDown
        onDoubleClick
      >
        <path d={path} markerEnd={edge.directed ? "url(#arrowhead)" : ""} />
        <path className="diagram-edge__hit" d={path} />
        {edge.label == ""
          ? React.null
          : <text
              x={centerX->number}
              y={(centerY -. 12.0)->number}
              className="diagram-edge__label"
              textAnchor="middle"
              fontSize={edge.fontSize->Int.toString}
            >
              {React.string(edge.label)}
            </text>}
        {selected
          ? <circle
              className="diagram-edge__bend"
              cx={centerX->number}
              cy={centerY->number}
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
    let (worldX, worldY) = screenToWorld(clientX, clientY, view)
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
    let (worldX, worldY) = screenToWorld(clientX, clientY, view)
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
    let (worldX, worldY) = screenToWorld(clientX, clientY, view)
    setGesture(_ => Resizing(shape.id, worldX, worldY, shape.w, shape.h, shape.x, shape.y))
  }

  let startBend = (event, edge: edge) => {
    event->ReactEvent.Pointer.stopPropagation
    let clientX = event->ReactEvent.Pointer.clientX->Int.toFloat
    let clientY = event->ReactEvent.Pointer.clientY->Int.toFloat
    let (worldX, worldY) = screenToWorld(clientX, clientY, view)
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
    let (worldX, worldY) = screenToWorld(clientX, clientY, view)
    switch gesture {
    | Idle => ()
    | Panning(startX, startY, panX, panY) =>
      onView({
        ...view,
        panX: panX +. clientX -. startX,
        panY: panY +. clientY -. startY,
      })
    | MovingShape(id, startX, startY, shapeX, shapeY) =>
      onBoard(
        BoardOps.updateShape(board, id, shape => {
          ...shape,
          x: shapeX +. worldX -. startX,
          y: shapeY +. worldY -. startY,
        }),
      )
    | Drawing(startX, startY, _endX, _endY) =>
      setGesture(_ => Drawing(startX, startY, worldX, worldY))
    | Resizing(id, startX, startY, startW, startH, _shapeX, _shapeY) =>
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
          let resized = {
            ...shape,
            w: width,
            h: height,
          }
          resized
        }),
      )
    | BendingEdge(id, offsetX, offsetY, _startCx, _startCy) =>
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
    let (worldX, worldY) = screenToWorld(clientX, clientY, view)
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
    let (worldX, worldY) = screenToWorld(clientX, clientY, view)
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

  let draftElement = switch gesture {
  | Drawing(startX, startY, endX, endY) =>
    let x = Math.min(startX, endX)
    let y = Math.min(startY, endY)
    let width = Math.abs(endX -. startX)
    let height = Math.abs(endY -. startY)
    <rect
      className="diagram-draft"
      x={x->number}
      y={y->number}
      width={width->number}
      height={height->number}
      rx={(tool == "circle" ? Math.max(width, height) /. 2.0 : 12.0)->number}
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
      <pattern id="grid" width="48" height="48" patternUnits="userSpaceOnUse">
        <path d="M 48 0 L 0 0 0 48" className="diagram-grid-line" />
      </pattern>
      <marker id="arrowhead" markerWidth="10" markerHeight="7" refX="9" refY="3.5" orient="auto">
        <polygon points="0 0, 10 3.5, 0 7" />
      </marker>
    </defs>
    <g
      transform={"translate(" ++
      (innerWidth /. 2.0 +. view.panX)->number ++
      " " ++
      (innerHeight /. 2.0 +. view.panY)->number ++
      ") scale(" ++
      view.zoom->number ++ ")"}
    >
      <rect
        x={-.innerWidth->number}
        y={-.innerHeight->number}
        width={(innerWidth *. 2.0)->number}
        height={(innerHeight *. 2.0)->number}
        fill="url(#grid)"
      />
      {board.layers
      ->Array.mapWithIndex((layer, index) =>
        layer.hidden
          ? React.null
          : <g key={layer.id} className="diagram-floor">
              <rect
                x="-560"
                y={(-320.0 -. index->Int.toFloat *. 26.0)->number}
                width="1120"
                height="640"
                rx="24"
                stroke={layer.color}
                opacity={view.activeLayer == index ? "0.52" : "0.18"}
              />
              <text x="-535" y={(-290.0 -. index->Int.toFloat *. 26.0)->number} fill={layer.color}>
                {React.string(layer.name)}
              </text>
            </g>
      )
      ->React.array}
      {board.edges
      ->Array.map(edge =>
        <EdgeView
          key={edge.id}
          edge
          board
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
      ->Array.map(shape =>
        <ShapeView
          key={shape.id}
          shape
          selected={selectedShapeId == Some(shape.id)}
          pending={pendingEdgeFrom == Some(shape.id)}
          onPointerDown={event => startShape(event, shape)}
          onDoubleClick={event => editShape(event, shape)}
          onResizePointerDown={event => startResize(event, shape)}
        />
      )
      ->React.array}
      {draftElement}
    </g>
  </svg>
}
