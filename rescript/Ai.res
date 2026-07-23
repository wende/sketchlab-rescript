open Model

let model = "gpt-5.5"
let endpoint = "https://api.openai.com/v1/responses"

type jsonFormat = {
  @as("type")
  type_: string,
}

type textConfig = {format: jsonFormat}

type requestPayload = {
  model: string,
  input: string,
  max_output_tokens: int,
  text: textConfig,
}

type requestOptions = {
  method: string,
  headers: dict<string>,
  body: string,
}

type response

@val
external fetch: (string, requestOptions) => promise<response> = "fetch"

@get
external ok: response => bool = "ok"

@get
external status: response => int = "status"

@send
external json: response => promise<JSON.t> = "json"

type outputPart = {text: option<string>}
type outputItem = {content: array<outputPart>}
type responsePayload = {output: array<outputItem>}

type generatedLayer = {name: string, color: string}
type generatedNode = {
  id: string,
  label: string,
  kind: string,
  icon: string,
  color: string,
  layer: int,
}
type generatedEdge = {
  from: string,
  @as("to")
  to_: string,
  label: string,
  directed: bool,
}
type generatedGraph = {
  name: string,
  layers: array<generatedLayer>,
  nodes: array<generatedNode>,
  edges: array<generatedEdge>,
}

external payloadToJson: requestPayload => JSON.t = "%identity"
external responseFromJson: JSON.t => responsePayload = "%identity"
external graphFromJson: JSON.t => generatedGraph = "%identity"
external boardToJson: board => JSON.t = "%identity"

let systemPrompt = (userPrompt, currentBoard) => {
  let context = switch currentBoard {
  | Some(board) =>
    "\nModify this current board and return the complete updated graph:\n" ++
    board->boardToJson->JSON.stringify
  | None => ""
  }
  `You generate concise architecture diagrams for Sketch Lab.
Return valid JSON only with this exact shape:
{"name":"Diagram name","layers":[{"name":"Ground","color":"#38bdf8"}],"nodes":[{"id":"api","label":"API","kind":"rect","icon":"","color":"#0f2740","layer":0}],"edges":[{"from":"api","to":"db","label":"queries","directed":true}]}
Rules:
- kind is rect, circle, icon, or text.
- Use 4-14 nodes unless asked otherwise.
- Every edge endpoint must reference a node id.
- Colors are #RRGGBB.
- Layers are bottom to top and every node layer is a valid index.
- Use directed edges for flows and dependencies.

User request:
${userPrompt}${context}`
}

let extractOutputText = payload => {
  let parsed = payload->responseFromJson
  parsed.output->Array.reduce(None, (found, item) =>
    switch found {
    | Some(_) => found
    | None =>
      item.content->Array.reduce(None, (text, part) =>
        switch text {
        | Some(_) => text
        | None => part.text
        }
      )
    }
  )
}

let validate = graph =>
  graph.name != "" &&
  graph.nodes->Array.length > 0 &&
  graph.nodes->Array.every(node =>
    node.id != "" &&
    node.label != "" &&
    ["rect", "circle", "icon", "text"]->Array.includes(node.kind)
  )

let graphToBoard = (graph: generatedGraph): board => {
  let columns = Math.ceil(graph.nodes->Array.length->Int.toFloat->Math.sqrt)->Float.toInt
  let rows = Math.ceil(graph.nodes->Array.length->Int.toFloat /. columns->Int.toFloat)->Float.toInt
  let layers =
    graph.layers->Array.length == 0
      ? [groundLayer]
      : graph.layers->Array.mapWithIndex((layer, index) => {
          id: "layer-" ++ index->Int.toString,
          name: layer.name,
          hidden: false,
          color: layer.color,
        })
  let shapes = graph.nodes->Array.mapWithIndex((node, index) => {
    let column = index % columns
    let row = index / columns
    let width = node.kind == "text" ? 240.0 : 150.0
    let height = node.kind == "text" ? 72.0 : 110.0
    let layer =
      node.layer < 0
        ? 0
        : node.layer >= layers->Array.length
        ? layers->Array.length - 1
        : node.layer
    {
      id: node.id,
      kind: node.kind,
      x: (column->Int.toFloat -. (columns - 1)->Int.toFloat /. 2.0) *. 610.0 -. width /. 2.0,
      y: (row->Int.toFloat -. (rows - 1)->Int.toFloat /. 2.0) *. 240.0 -. height /. 2.0,
      w: width,
      h: height,
      label: node.label,
      fill: node.kind == "text" ? noFill : node.color,
      layer,
      fontSize: 48,
      src: None,
      icon: node.icon == "" ? None : Some(node.icon),
    }
  })
  let nodeIds = shapes->Array.map(shape => shape.id)
  let edges =
    graph.edges
    ->Array.filter(edge => nodeIds->Array.includes(edge.from) && nodeIds->Array.includes(edge.to_))
    ->Array.map(edge => {
      id: Model.uid(),
      from: edge.from,
      to_: edge.to_,
      label: edge.label,
      directed: edge.directed,
      fontSize: 48,
      cx: None,
      cy: None,
    })
  let now = Date.now()
  {
    id: Model.uid(),
    name: graph.name,
    shapes,
    edges,
    layers,
    createdAt: now,
    updatedAt: now,
  }
}

let boardFromGeneratedGraphJson = text =>
  try {
    let graph = text->JSON.parseOrThrow->graphFromJson
    validate(graph) ? Some(graphToBoard(graph)) : None
  } catch {
  | _ => None
  }

let generate = async (~apiKey, ~prompt, ~currentBoard: option<board>=None): result<
  board,
  string,
> => {
  let key = apiKey->String.trim
  let request = prompt->String.trim
  if key == "" {
    Error("Enter an OpenAI API key.")
  } else if request == "" {
    Error("Describe the diagram you want to generate.")
  } else {
    let headers = dict{
      "Content-Type": "application/json",
      "Authorization": "Bearer " ++ key,
    }
    let payload: requestPayload = {
      model,
      input: systemPrompt(request, currentBoard),
      max_output_tokens: 4096,
      text: {format: {type_: "json_object"}},
    }
    try {
      let response = await fetch(
        endpoint,
        {
          method: "POST",
          headers,
          body: payload->payloadToJson->JSON.stringify,
        },
      )
      if !(response->ok) {
        Error(
          response->status == 401
            ? "OpenAI rejected the API key."
            : "OpenAI request failed (" ++ response->status->Int.toString ++ ").",
        )
      } else {
        let payload = await response->json
        switch extractOutputText(payload) {
        | None => Error("OpenAI did not return diagram JSON.")
        | Some(text) =>
          try {
            switch boardFromGeneratedGraphJson(text) {
            | Some(board) => Ok(board)
            | None => Error("OpenAI returned an invalid diagram.")
            }
          } catch {
          | _ => Error("OpenAI returned invalid diagram JSON.")
          }
        }
      }
    } catch {
    | _ => Error("Could not reach OpenAI. Check your connection and key.")
    }
  }
}
