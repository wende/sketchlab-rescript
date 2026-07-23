open Model

external styleFromDict: dict<string> => ReactDOMStyle.t = "%identity"

type fileList
type file
type fileReader

@get @return(nullable)
external inputFiles: Browser.inputTarget => option<fileList> = "files"

@send @return(nullable)
external fileAt: (fileList, int) => option<file> = "item"

@new
external makeFileReader: unit => fileReader = "FileReader"

@set
external setReaderOnLoad: (fileReader, unit => unit) => unit = "onload"

@get @return(nullable)
external readerResult: fileReader => option<string> = "result"

@send
external readAsDataUrl: (fileReader, file) => unit = "readAsDataURL"

type toolDef = {
  name: string,
  label: string,
  key: string,
  glyph: string,
}

let tools: array<toolDef> = [
  {name: "select", label: "Select / move", key: "V", glyph: "↖"},
  {name: "text", label: "Text", key: "T", glyph: "T"},
  {name: "code", label: "Code panel", key: "C", glyph: "</>"},
  {name: "rect", label: "Rectangle", key: "R", glyph: "▭"},
  {name: "circle", label: "Circle", key: "O", glyph: "○"},
  {name: "line", label: "Connector line", key: "L", glyph: "╱"},
  {name: "arrow", label: "Arrow", key: "A", glyph: "↗"},
  {name: "hand", label: "Move / pan", key: "M", glyph: "✥"},
]

let fills = [
  "transparent",
  "#0f2740",
  "#1e293b",
  "#0c4a6e",
  "#7f1d1d",
  "#14532d",
  "#5b21b6",
  "#e2e8f0",
  "#fbbf24",
  "#fb923c",
  "#4ade80",
  "#38bdf8",
  "#f9a8d4",
]

let architectureIcons = [
  ("Browser", "🌐"),
  ("User", "👤"),
  ("Mobile", "📱"),
  ("Cloud", "☁"),
  ("Server", "▥"),
  ("Database", "◉"),
  ("Cache", "⚡"),
  ("Queue", "≋"),
  ("API", "⇄"),
  ("Gateway", "◇"),
  ("Function", "λ"),
  ("Container", "⬡"),
  ("Cluster", "✣"),
  ("Network", "⌘"),
  ("Firewall", "▰"),
  ("Lock", "🔒"),
  ("Key", "⚿"),
  ("Storage", "▱"),
  ("File", "▤"),
  ("Email", "✉"),
  ("Search", "⌕"),
  ("Analytics", "▥"),
  ("Monitor", "▣"),
  ("Webhook", "↪"),
  ("Load Balancer", "⚖"),
  ("CDN", "◎"),
  ("DNS", "⌁"),
  ("Router", "⇆"),
  ("Switch", "⇌"),
  ("Proxy", "⧉"),
  ("Microservice", "⬢"),
  ("Worker", "⚒"),
  ("Scheduler", "◷"),
  ("Cron", "◴"),
  ("PubSub", "☊"),
  ("Topic", "≡"),
  ("Stream", "≈"),
  ("Event", "⚑"),
  ("Log", "▥"),
  ("Metric", "∿"),
  ("Trace", "⤳"),
  ("Dashboard", "▦"),
  ("Alert", "⚠"),
  ("Shield", "⛨"),
  ("Certificate", "✧"),
  ("Secret", "◆"),
  ("Identity", "◇"),
  ("OAuth", "⎋"),
  ("Kubernetes", "⎈"),
  ("Docker", "▣"),
  ("Virtual Machine", "▤"),
  ("Bare Metal", "▰"),
  ("Lambda", "λ"),
  ("Edge", "◫"),
  ("Region", "◈"),
  ("Availability Zone", "◇"),
  ("VPC", "⬡"),
  ("Subnet", "▱"),
  ("Internet", "☍"),
  ("WiFi", "⌁"),
  ("Bluetooth", "ᛒ"),
  ("IoT", "⌂"),
  ("Sensor", "◉"),
  ("Robot", "⚙"),
  ("Payment", "$"),
  ("Shopping Cart", "🛒"),
  ("Billing", "¤"),
  ("CRM", "♙"),
  ("ERP", "▦"),
  ("Git Branch", "⑂"),
  ("Repository", "◫"),
  ("Pipeline", "⤇"),
  ("Build", "⚒"),
  ("Deploy", "↥"),
  ("Package", "▣"),
  ("Artifact", "◇"),
  ("Test", "✓"),
  ("Feature Flag", "⚑"),
  ("Configuration", "⚙"),
  ("Terminal", ">_"),
  ("Code", "{}"),
  ("Document", "▤"),
  ("Folder", "▰"),
  ("Archive", "▥"),
  ("Backup", "↶"),
  ("Replication", "⧉"),
  ("Shard", "⋮"),
  ("Table", "▦"),
  ("Data Lake", "≋"),
  ("Warehouse", "▥"),
  ("ML Model", "⌬"),
  ("Artificial Intelligence", "✦"),
]

let selectedShape = (board, selection) =>
  switch selection {
  | ShapeSelection(id) => BoardOps.findShape(board, id)
  | _ => None
  }

module BoardDrawer = {
  @react.component
  let make = (
    ~open_: bool,
    ~boards: array<boardMeta>,
    ~activeId,
    ~onClose,
    ~onNew,
    ~onSelect,
    ~onDelete,
  ) =>
    open_
      ? <div className="board-drawer-shell">
          <div className="board-drawer__backdrop is-visible" onClick={_ => onClose()} />
          <aside className="board-drawer is-open" role="dialog" ariaLabel="Your boards">
            <div className="board-drawer__header">
              <h2> {React.string("Boards")} </h2>
              <button
                className="board-drawer__close"
                type_="button"
                ariaLabel="Close boards menu"
                onClick={_ => onClose()}
              >
                {React.string("×")}
              </button>
            </div>
            <button
              className="btn btn--accent board-drawer__new" type_="button" onClick={_ => onNew()}
            >
              {React.string("+ New board")}
            </button>
            <div className="board-drawer__list">
              {boards->Array.length == 0
                ? <p className="board-drawer__empty"> {React.string("No boards yet.")} </p>
                : boards
                  ->Array.map(meta =>
                    <div
                      key={meta.id}
                      className="board-drawer__item"
                      ariaCurrent={meta.id == activeId ? #"true" : #"false"}
                      role="button"
                      tabIndex={0}
                      onClick={_ => onSelect(meta.id)}
                    >
                      <div className="board-drawer__thumb board-drawer__thumb--empty">
                        {React.string("✎")}
                      </div>
                      <div className="board-drawer__meta">
                        <div className="board-drawer__name"> {React.string(meta.name)} </div>
                        <div className="board-drawer__sub">
                          {React.string(
                            meta.shapeCount->Int.toString ++
                            (meta.shapeCount == 1 ? " shape · " : " shapes · ") ++
                            Browser.formatDate(meta.updatedAt),
                          )}
                        </div>
                      </div>
                      <button
                        className="board-drawer__delete"
                        type_="button"
                        ariaLabel={"Delete " ++ meta.name}
                        onClick={event => {
                          event->ReactEvent.Mouse.stopPropagation
                          onDelete(meta.id, meta.name)
                        }}
                      >
                        {React.string("🗑")}
                      </button>
                    </div>
                  )
                  ->React.array}
            </div>
          </aside>
        </div>
      : React.null
}

module IconPalette = {
  @react.component
  let make = (~open_: bool, ~onClose, ~onPick) => {
    let (query, setQuery) = React.useState(() => "")
    let normalized = query->String.trim->String.toLowerCase
    let matches =
      normalized == ""
        ? architectureIcons
        : architectureIcons->Array.filter(((name, _glyph)) =>
            name->String.toLowerCase->String.includes(normalized)
          )
    open_
      ? <div className="icon-palette is-open" role="dialog" ariaModal=true ariaLabel="Insert icon">
          <div className="icon-palette__card">
            <div className="icon-palette__head">
              <input
                className="icon-palette__search"
                value={query}
                placeholder="Search architecture icons"
                ariaLabel="Search icons"
                autoFocus=true
                onChange={event => setQuery(_ => event->Browser.formTarget->Browser.targetValue)}
              />
              <button
                className="icon-palette__close"
                type_="button"
                ariaLabel="Close icon palette"
                onClick={_ => onClose()}
              >
                {React.string("×")}
              </button>
            </div>
            <div className="icon-palette__list">
              {matches
              ->Array.map(((name, glyph)) =>
                <button
                  key={name}
                  className="icon-row"
                  type_="button"
                  onClick={_ => {
                    onPick(name, glyph)
                    setQuery(_ => "")
                  }}
                >
                  <span className="icon-row__preview"> {React.string(glyph)} </span>
                  <span className="icon-row__name"> {React.string(name)} </span>
                </button>
              )
              ->React.array}
            </div>
          </div>
        </div>
      : React.null
  }
}

module LayersPanel = {
  @react.component
  let make = (
    ~open_: bool,
    ~board: board,
    ~view: view,
    ~selection: selection,
    ~onBoard,
    ~onView,
    ~onClose,
  ) =>
    <aside
      className={open_ ? "layers-panel" : "layers-panel is-collapsed"}
      role="region"
      ariaLabel="Layers"
    >
      <div className="layers-panel__header">
        <h2> {React.string("Layers")} </h2>
        <div className="layers-panel__header-actions">
          <button
            className="layers-panel__add"
            type_="button"
            ariaLabel="Add a floor"
            onClick={_ => {
              let (next, index) = BoardOps.addLayer(board)
              onBoard(next)
              onView({...view, activeLayer: index})
            }}
          >
            {React.string("+")}
          </button>
          <button
            className="layers-panel__collapse"
            type_="button"
            ariaLabel="Hide the layers panel"
            onClick={_ => onClose()}
          >
            {React.string("›")}
          </button>
        </div>
      </div>
      <div className="layers-panel__list">
        {board.layers
        ->Array.mapWithIndex((layer, index) => (layer, index))
        ->Array.toReversed
        ->Array.map(((layer, index)) => {
          let count = BoardOps.layerItemCount(board, index)
          <div
            key={layer.id}
            className={layer.hidden ? "layers-panel__item is-hidden" : "layers-panel__item"}
            ariaCurrent={view.activeLayer == index ? #"true" : #"false"}
            role="button"
            tabIndex={0}
            onClick={_ => {
              let revealed = layer.hidden
                ? BoardOps.updateLayer(board, index, item => {...item, hidden: false})
                : board
              if revealed != board {
                onBoard(revealed)
              }
              onView({...view, activeLayer: index})
            }}
          >
            <div className="layers-panel__top">
              <button
                className="layers-panel__eye"
                type_="button"
                ariaLabel={layer.hidden ? "Show this floor" : "Hide this floor"}
                onClick={event => {
                  event->ReactEvent.Mouse.stopPropagation
                  onBoard(
                    BoardOps.updateLayer(board, index, item => {
                      ...item,
                      hidden: !item.hidden,
                    }),
                  )
                }}
              >
                {React.string(layer.hidden ? "◌" : "◉")}
              </button>
              <span
                className="layers-panel__color"
                style={dict{"background": layer.color}->styleFromDict}
              />
              <div className="layers-panel__name"> {React.string(layer.name)} </div>
            </div>
            <div className="layers-panel__sub">
              {React.string(
                "Floor " ++
                index->Int.toString ++
                " · " ++
                count->Int.toString ++ (count == 1 ? " item" : " items"),
              )}
              <div className="layers-panel__actions">
                <button
                  className="layers-panel__action"
                  type_="button"
                  disabled={selection == NoSelection}
                  title="Move selection to this floor"
                  onClick={event => {
                    event->ReactEvent.Mouse.stopPropagation
                    onBoard(BoardOps.moveSelectionToLayer(board, selection, index))
                  }}
                >
                  {React.string("⤵")}
                </button>
                <button
                  className="layers-panel__action"
                  type_="button"
                  title="Rename floor"
                  onClick={event => {
                    event->ReactEvent.Mouse.stopPropagation
                    switch Browser.prompt("Rename layer", layer.name) {
                    | Some(name) =>
                      onBoard(BoardOps.updateLayer(board, index, item => {...item, name}))
                    | None => ()
                    }
                  }}
                >
                  {React.string("✎")}
                </button>
                <button
                  className="layers-panel__action layers-panel__action--danger"
                  type_="button"
                  disabled={board.layers->Array.length <= 1}
                  title="Delete floor"
                  onClick={event => {
                    event->ReactEvent.Mouse.stopPropagation
                    if Browser.confirm("Delete this floor and move its items down?") {
                      onBoard(BoardOps.deleteLayer(board, index))
                      onView({...view, activeLayer: 0})
                    }
                  }}
                >
                  {React.string("🗑")}
                </button>
              </div>
            </div>
          </div>
        })
        ->React.array}
      </div>
      <label
        className={board.layers->Array.length < 2
          ? "layers-panel__spread is-disabled"
          : "layers-panel__spread"}
      >
        <span> {React.string("Floor spread")} </span>
        <input
          type_="range"
          min="8"
          max="80"
          value="26"
          disabled={board.layers->Array.length < 2}
          ariaLabel="Floor spread"
        />
      </label>
    </aside>
}

module HelpModal = {
  @react.component
  let make = (~kind: string, ~onClose) => {
    let isSkill = kind == "skill"
    <div className="controls-help is-open" onClick={_ => onClose()}>
      <div
        className={isSkill ? "skill-help__card" : "controls-help__card"}
        role="dialog"
        ariaModal=true
        ariaLabel={isSkill ? "Skill" : "Controls"}
        onClick={event => event->ReactEvent.Mouse.stopPropagation}
      >
        <div className={isSkill ? "skill-help__head" : "controls-help__head"}>
          <h2> {React.string(isSkill ? "Skill" : "Controls")} </h2>
          <button
            className={isSkill ? "skill-help__close" : "controls-help__close"}
            type_="button"
            ariaLabel="Close"
            onClick={_ => onClose()}
          >
            {React.string("✕")}
          </button>
        </div>
        {isSkill
          ? <div className="skill-help__body">
              <p>
                {React.string(
                  "Give this skill file to an AI coding agent so it can generate Sketch Lab diagrams.",
                )}
              </p>
              <div className="skill-help__url-row">
                <code className="skill-help__url">
                  {React.string("https://sketchlab.webdevcody.com/skills/sketch-lab/SKILL.md")}
                </code>
              </div>
            </div>
          : <div className="controls-help__rows">
              {[
                ("Pan", "Scroll · Space + drag · Hand tool (M)"),
                ("Zoom", "Mouse wheel · + and − buttons"),
                ("Create", "R rectangle · O circle · T text · C code"),
                ("Connect", "L line · A arrow, then click two shapes"),
                ("Edit", "Double-click a shape or connector"),
                ("Undo", "⌘/Ctrl+Z · redo with Shift+⌘/Ctrl+Z"),
              ]
              ->Array.map(((action, how)) =>
                <div key={action} className="controls-help__row">
                  <div className="controls-help__action"> {React.string(action)} </div>
                  <div className="controls-help__how"> {React.string(how)} </div>
                </div>
              )
              ->React.array}
            </div>}
      </div>
    </div>
  }
}

module AiPanel = {
  @react.component
  let make = (~open_: bool, ~board: board, ~onClose, ~onGenerated) => {
    let (apiKey, setApiKey) = React.useState(() => "")
    let (prompt, setPrompt) = React.useState(() => "")
    let (mode, setMode) = React.useState(() => "generate")
    let (loading, setLoading) = React.useState(() => false)
    let (error, setError) = React.useState(() => "")

    let submit = event => {
      event->ReactEvent.Form.preventDefault
      if !loading {
        setLoading(_ => true)
        setError(_ => "")
        let run = async () => {
          let result = await Ai.generate(
            ~apiKey,
            ~prompt,
            ~currentBoard=mode == "modify" ? Some(board) : None,
          )
          switch result {
          | Ok(generated) =>
            onGenerated(generated, mode == "modify")
            setPrompt(_ => "")
            onClose()
          | Error(message) => setError(_ => message)
          }
          setLoading(_ => false)
        }
        run()->ignore
      }
    }

    open_
      ? <div className="ai-panel-backdrop" onPointerDown={_ => loading ? () : onClose()}>
          <section
            className="ai-panel"
            role="dialog"
            ariaModal=true
            ariaLabel="Generate diagram with AI"
            onPointerDown={event => event->ReactEvent.Pointer.stopPropagation}
          >
            <header className="ai-panel__header">
              <div>
                <h2> {React.string("Generate Diagram")} </h2>
                <p>
                  {React.string(
                    "Your API key is sent directly from this browser to OpenAI and is not stored.",
                  )}
                </p>
              </div>
              <button
                className="ai-panel__close"
                type_="button"
                ariaLabel="Close AI generator"
                disabled={loading}
                onClick={_ => onClose()}
              >
                {React.string("×")}
              </button>
            </header>
            <form className="ai-panel__form" ariaBusy={loading} onSubmit={submit}>
              <div className="ai-panel__modes" role="group" ariaLabel="AI diagram mode">
                <button
                  className={mode == "generate" ? "ai-panel__mode is-active" : "ai-panel__mode"}
                  type_="button"
                  ariaPressed={mode == "generate" ? #"true" : #"false"}
                  disabled={loading}
                  onClick={_ => setMode(_ => "generate")}
                >
                  {React.string("Generate new")}
                </button>
                <button
                  className={mode == "modify" ? "ai-panel__mode is-active" : "ai-panel__mode"}
                  type_="button"
                  ariaPressed={mode == "modify" ? #"true" : #"false"}
                  disabled={loading || board.shapes->Array.length == 0}
                  onClick={_ => setMode(_ => "modify")}
                >
                  {React.string("Modify current")}
                </button>
              </div>
              <label className="ai-panel__field">
                <span> {React.string("OpenAI API key")} </span>
                <input
                  className="ai-panel__input"
                  type_="password"
                  value={apiKey}
                  autoComplete="off"
                  placeholder="sk-..."
                  disabled={loading}
                  onChange={event => setApiKey(_ => event->Browser.formTarget->Browser.targetValue)}
                />
              </label>
              <label className="ai-panel__field">
                <span> {React.string("Prompt")} </span>
                <textarea
                  className="ai-panel__textarea"
                  rows={5}
                  value={prompt}
                  placeholder={mode == "modify"
                    ? "Describe how to change the current diagram."
                    : "Describe the architecture diagram you want."}
                  disabled={loading}
                  onChange={event => setPrompt(_ => event->Browser.formTarget->Browser.targetValue)}
                />
              </label>
              <div
                className={error == "" ? "ai-panel__error" : "ai-panel__error is-visible"}
                role="alert"
              >
                {React.string(error)}
              </div>
              <div className="ai-panel__footer">
                <span className="ai-panel__hint">
                  {React.string("Structured diagram generation")}
                </span>
                <button className="btn btn--accent" type_="submit" disabled={loading}>
                  {React.string(
                    loading
                      ? "Working…"
                      : (mode == "modify" ? "Modify with " : "Generate with ") ++ Ai.model,
                  )}
                </button>
              </div>
            </form>
          </section>
        </div>
      : React.null
  }
}

@react.component
let make = (~initialBoard: board, ~initialShared: bool) => {
  let (board, setBoard) = React.useState(() => initialBoard)
  let (shared, setShared) = React.useState(() => initialShared)
  let (tool, setTool) = React.useState(() => "select")
  let (spacePressed, setSpacePressed) = React.useState(() => false)
  let (selection, setSelection) = React.useState(() => NoSelection)
  let (view, setView) = React.useState(() => defaultView)
  let (fill, setFill) = React.useState(() => "#0f2740")
  let (fontSize, setFontSize) = React.useState(() => 24)
  let (drawerOpen, setDrawerOpen) = React.useState(() => false)
  let (layersOpen, setLayersOpen) = React.useState(() => true)
  let (settingsOpen, setSettingsOpen) = React.useState(() => false)
  let (aiOpen, setAiOpen) = React.useState(() => false)
  let (iconOpen, setIconOpen) = React.useState(() => false)
  let (help, setHelp) = React.useState(() => None)
  let (shareLink, setShareLink) = React.useState(() => None)
  let (boardList, setBoardList) = React.useState((): array<boardMeta> => [])
  let (toast, setToast) = React.useState(() => "")
  let past = React.useRef([])
  let future = React.useRef([])
  let clipboardShape = React.useRef((None: option<shape>))

  let showToast = message => {
    setToast(_ => message)
    let _timer = Browser.setTimeout(() => setToast(_ => ""), 1800)
  }

  let replaceBoard = next => {
    past.current->Array.push(board)->ignore
    future.current = []
    setBoard(_ => next)
  }

  let undo = () =>
    switch past.current->Array.pop {
    | Some(previous) =>
      future.current->Array.push(board)->ignore
      setBoard(_ => previous)
      setSelection(_ => NoSelection)
    | None => ()
    }

  let redo = () =>
    switch future.current->Array.pop {
    | Some(next) =>
      past.current->Array.push(board)->ignore
      setBoard(_ => next)
      setSelection(_ => NoSelection)
    | None => ()
    }

  let refreshBoards = async () => {
    let metas = await Persistence.boardMetas()
    setBoardList(_ => metas)
  }

  React.useEffect1(() => {
    if shared {
      None
    } else {
      let timer = Browser.setTimeout(() => {
        Persistence.saveBoard(board)->ignore
        refreshBoards()->ignore
      }, 350)
      Some(() => Browser.clearTimeout(timer))
    }
  }, [board])

  React.useEffect2(() => {
    let onKey = event => {
      let key = event->Browser.key->String.toLowerCase
      let command = event->Browser.metaKey || event->Browser.ctrlKey
      if event->Browser.isTypingEvent {
        ()
      } else if key == " " {
        event->Browser.preventDefault
        setSpacePressed(_ => true)
      } else if command && key == "z" {
        event->Browser.preventDefault
        event->Browser.shiftKey ? redo() : undo()
      } else if command && key == "y" {
        event->Browser.preventDefault
        redo()
      } else if command && key == "c" {
        event->Browser.preventDefault
        clipboardShape.current = selectedShape(board, selection)
      } else if command && key == "x" {
        event->Browser.preventDefault
        clipboardShape.current = selectedShape(board, selection)
        replaceBoard(BoardOps.deleteSelection(board, selection))
        setSelection(_ => NoSelection)
      } else if command && key == "v" {
        event->Browser.preventDefault
        switch clipboardShape.current {
        | Some(copied) =>
          let pasted = {
            ...copied,
            id: Model.uid(),
            x: copied.x +. 32.0,
            y: copied.y +. 32.0,
          }
          replaceBoard(BoardOps.addShape(board, pasted))
          setSelection(_ => ShapeSelection(pasted.id))
        | None => ()
        }
      } else if key == "delete" || key == "backspace" {
        replaceBoard(BoardOps.deleteSelection(board, selection))
        setSelection(_ => NoSelection)
      } else {
        switch key {
        | "v" => setTool(_ => "select")
        | "t" => setTool(_ => "text")
        | "c" => setTool(_ => "code")
        | "r" => setTool(_ => "rect")
        | "o" => setTool(_ => "circle")
        | "l" => setTool(_ => "line")
        | "a" => setTool(_ => "arrow")
        | "m" => setTool(_ => "hand")
        | "/" => setIconOpen(_ => true)
        | "?" => setHelp(_ => Some("controls"))
        | _ => ()
        }
      }
    }
    let onKeyUp = event => {
      if event->Browser.key == " " {
        setSpacePressed(_ => false)
      }
    }
    Browser.window->Browser.addKeyboardListener("keydown", onKey)
    Browser.window->Browser.addKeyboardListener("keyup", onKeyUp)
    Some(
      () => {
        Browser.window->Browser.removeKeyboardListener("keydown", onKey)
        Browser.window->Browser.removeKeyboardListener("keyup", onKeyUp)
      },
    )
  }, (board, selection))

  let createBoard = async () => {
    if !shared {
      await Persistence.saveBoard(board)
    }
    let next = Model.emptyBoard()
    await Persistence.saveBoard(next)
    setBoard(_ => next)
    setShared(_ => false)
    setSelection(_ => NoSelection)
    setView(_ => defaultView)
    Browser.navigateBoard(next.id)
    setDrawerOpen(_ => false)
    await refreshBoards()
  }

  let switchBoard = async id => {
    switch await Persistence.findBoard(id) {
    | Some(next) =>
      setBoard(_ => next)
      setShared(_ => false)
      setSelection(_ => NoSelection)
      setView(_ => defaultView)
      Browser.navigateBoard(next.id)
    | None => ()
    }
    setDrawerOpen(_ => false)
  }

  let deleteBoard = async (id, name) => {
    if Browser.confirm("Delete \"" ++ name ++ "\"? This cannot be undone.") {
      await Persistence.deleteBoard(id)
      let boards = await Persistence.loadBoards()
      if id == board.id {
        let next = switch boards->Array.get(0) {
        | Some(item) => item
        | None =>
          let starter = Model.starterBoard()
          await Persistence.saveBoard(starter)
          starter
        }
        setBoard(_ => next)
        Browser.navigateBoard(next.id)
      }
      await refreshBoards()
    }
  }

  let saveCopy = async () => {
    await Persistence.saveBoard(board)
    setShared(_ => false)
    Browser.navigateBoard(board.id)
    showToast("Saved a local copy")
    await refreshBoards()
  }

  let share = () => {
    setShareLink(_ => Some(Browser.shareUrl(board)))
  }

  let selected = selectedShape(board, selection)

  let pickImage = event =>
    switch event->Browser.formTarget->inputFiles {
    | Some(files) =>
      switch files->fileAt(0) {
      | Some(file) =>
        let reader = makeFileReader()
        reader->setReaderOnLoad(() =>
          switch reader->readerResult {
          | Some(src) =>
            let image = Model.makeShape(
              ~kind="image",
              ~x=-.view.panX /. view.zoom -. 120.0,
              ~y=-.view.panY /. view.zoom -. 90.0,
              ~w=240.0,
              ~h=180.0,
              ~fill=noFill,
              ~layer=view.activeLayer,
              ~src,
              (),
            )
            replaceBoard(BoardOps.addShape(board, image))
            setSelection(_ => ShapeSelection(image.id))
          | None => ()
          }
        )
        reader->readAsDataUrl(file)
      | None => ()
      }
    | None => ()
    }

  <div className={drawerOpen ? "editor editor--drawer-open" : "editor"}>
    <BoardCanvas
      board
      view
      tool
      spacePressed
      fill
      fontSize
      selection
      onBoard={replaceBoard}
      onView={next => setView(_ => next)}
      onSelection={next => setSelection(_ => next)}
      onTool={next => setTool(_ => next)}
    />

    <header className="topbar">
      <button
        className="btn btn--icon topbar__menu"
        type_="button"
        ariaLabel="Boards"
        ariaExpanded={drawerOpen}
        onClick={_ => {
          refreshBoards()->ignore
          setDrawerOpen(open_ => !open_)
        }}
      >
        {React.string("☰")}
      </button>
      <div className="topbar__slide">
        <div className="topbar__group">
          <button
            className="btn btn--icon"
            type_="button"
            ariaLabel="Undo"
            disabled={past.current->Array.length == 0}
            onClick={_ => undo()}
          >
            {React.string("↶")}
          </button>
          <button
            className="btn btn--icon"
            type_="button"
            ariaLabel="Redo"
            disabled={future.current->Array.length == 0}
            onClick={_ => redo()}
          >
            {React.string("↷")}
          </button>
        </div>
        <input
          className="topbar__name"
          value={board.name}
          spellCheck=false
          ariaLabel="Board name"
          onChange={event => {
            let name = event->Browser.formTarget->Browser.targetValue
            replaceBoard(BoardOps.rename(board, name))
          }}
        />
        <div className="topbar__spacer" />
        {shared
          ? <button className="btn btn--accent" type_="button" onClick={_ => saveCopy()->ignore}>
              {React.string("Save a copy")}
            </button>
          : React.null}
        <button
          className="btn btn--icon"
          type_="button"
          ariaLabel="Generate or modify with AI"
          title="Generate or modify with AI"
          onClick={_ => setAiOpen(_ => true)}
        >
          {React.string("✦")}
        </button>
        <button
          className="btn btn--icon"
          type_="button"
          ariaLabel="Auto layout"
          title="Auto layout"
          onClick={_ => {
            replaceBoard(BoardOps.autoLayout(board))
            showToast("Auto layout applied")
          }}
        >
          {React.string("⌘")}
        </button>
        <button
          className="btn btn--icon"
          type_="button"
          ariaLabel="Toggle layers panel"
          ariaExpanded={layersOpen}
          onClick={_ => {
            setLayersOpen(open_ => !open_)
            setSettingsOpen(_ => false)
          }}
        >
          {React.string("▱")}
        </button>
        <button
          className="btn btn--icon"
          type_="button"
          ariaLabel="Toggle view controls panel"
          ariaExpanded={settingsOpen}
          onClick={_ => {
            setSettingsOpen(open_ => !open_)
            setLayersOpen(_ => false)
          }}
        >
          {React.string("⚙")}
        </button>
        <a
          className="btn btn--icon"
          href="https://github.com/wende/sketchlab-rescript"
          target="_blank"
          rel="noopener noreferrer"
          ariaLabel="Sketch Lab on GitHub"
        >
          {React.string("◖◗")}
        </a>
        <button
          className="btn btn--icon"
          type_="button"
          ariaLabel="Skill"
          onClick={_ => setHelp(_ => Some("skill"))}
        >
          {React.string("▣")}
        </button>
        <button className="btn btn--accent" type_="button" onClick={_ => share()}>
          {React.string("Share")}
        </button>
      </div>
    </header>

    {shared
      ? <div className="banner">
          {React.string("You're viewing a shared board. Save a copy to edit and keep it.")}
        </div>
      : React.null}

    <div className="toolbar">
      {tools
      ->Array.map(item =>
        <button
          key={item.name}
          className={tool == item.name ? "tool is-active" : "tool"}
          type_="button"
          title={item.label ++ " (" ++ item.key ++ ")"}
          ariaLabel={item.label}
          onClick={_ => setTool(_ => item.name)}
        >
          <span className="tool__glyph"> {React.string(item.glyph)} </span>
          <span className="tool__kbd"> {React.string(item.key)} </span>
        </button>
      )
      ->React.array}
      <div className="toolbar__sep" />
      <button
        className="tool"
        type_="button"
        title="Insert icon (/)"
        ariaLabel="Insert icon"
        onClick={_ => setIconOpen(_ => true)}
      >
        <span className="tool__glyph"> {React.string("✦")} </span>
        <span className="tool__kbd"> {React.string("/")} </span>
      </button>
      <label className="tool image-tool" title="Insert image">
        <span className="tool__glyph"> {React.string("▧")} </span>
        <span className="tool__kbd"> {React.string("I")} </span>
        <input
          className="image-tool__input"
          type_="file"
          accept="image/*"
          ariaLabel="Insert image"
          onChange={pickImage}
        />
      </label>
    </div>

    <div className="zoombar">
      <button
        className="btn btn--icon"
        type_="button"
        ariaLabel="Zoom out"
        onClick={_ =>
          setView(current => {
            ...current,
            zoom: BoardOps.clampZoom(current.zoom /. 1.2),
          })}
      >
        {React.string("−")}
      </button>
      <span className="zoombar__label">
        {React.string(Math.round(view.zoom *. 100.0)->Float.toInt->Int.toString ++ "%")}
      </span>
      <button
        className="btn btn--icon"
        type_="button"
        ariaLabel="Zoom in"
        onClick={_ =>
          setView(current => {
            ...current,
            zoom: BoardOps.clampZoom(current.zoom *. 1.2),
          })}
      >
        {React.string("+")}
      </button>
      <button className="btn" type_="button" onClick={_ => setView(_ => defaultView)}>
        {React.string("Fit")}
      </button>
      <div className="zoombar__sep" />
      <button
        className="btn btn--icon"
        type_="button"
        ariaLabel="Controls and shortcuts"
        onClick={_ => setHelp(_ => Some("controls"))}
      >
        {React.string("?")}
      </button>
    </div>

    <div className={selection == NoSelection ? "style-panel" : "style-panel style-panel--editing"}>
      <div className="field">
        <span> {React.string("Fill")} </span>
        <div className="rescript-swatches">
          {fills
          ->Array.map(color =>
            <button
              key={color}
              type_="button"
              className={color == fill ? "swatch is-active" : "swatch"}
              title={color == noFill ? "No fill" : color}
              style={(color == noFill ? dict{} : dict{"background": color})->styleFromDict}
              onClick={_ => {
                setFill(_ => color)
                switch selected {
                | Some(shape) =>
                  replaceBoard(
                    BoardOps.updateShape(board, shape.id, current => {
                      ...current,
                      fill: color,
                    }),
                  )
                | None => ()
                }
              }}
            />
          )
          ->React.array}
        </div>
      </div>
      <div className="field">
        <span> {React.string("Size")} </span>
        <div className="size-picker">
          {[16, 20, 24, 32]
          ->Array.mapWithIndex((size, index) =>
            <button
              key={size->Int.toString}
              className={fontSize == size ? "size-btn is-active" : "size-btn"}
              type_="button"
              onClick={_ => {
                setFontSize(_ => size)
                replaceBoard(BoardOps.setAllFontSizes(board, size))
              }}
            >
              {React.string(["S", "M", "L", "XL"]->Array.getUnsafe(index))}
            </button>
          )
          ->React.array}
        </div>
      </div>
      <button
        className="btn btn--danger"
        type_="button"
        disabled={selection == NoSelection}
        onClick={_ => {
          replaceBoard(BoardOps.deleteSelection(board, selection))
          setSelection(_ => NoSelection)
        }}
      >
        {React.string("Delete")}
      </button>
    </div>

    <LayersPanel
      open_={layersOpen}
      board
      view
      selection
      onBoard={replaceBoard}
      onView={next => setView(_ => next)}
      onClose={() => setLayersOpen(_ => false)}
    />

    {settingsOpen
      ? <aside className="settings-panel" role="region" ariaLabel="View controls">
          <div className="settings-panel__header">
            <h2> {React.string("View controls")} </h2>
            <button
              className="settings-panel__collapse"
              type_="button"
              ariaLabel="Hide view controls"
              onClick={_ => setSettingsOpen(_ => false)}
            >
              {React.string("›")}
            </button>
          </div>
          <div className="settings-panel__section">
            <label className="settings-panel__slider">
              <strong> {React.string("Zoom")} </strong>
              <span className="settings-panel__toggle-sub">
                {React.string("Canvas magnification")}
              </span>
              <input
                className="settings-panel__range"
                type_="range"
                min="20"
                max="400"
                value={Math.round(view.zoom *. 100.0)->Float.toInt->Int.toString}
                onChange={event => {
                  let raw = event->Browser.formTarget->Browser.targetValue
                  switch raw->Float.fromString {
                  | Some(value) => setView(current => {...current, zoom: value /. 100.0})
                  | None => ()
                  }
                }}
              />
            </label>
          </div>
        </aside>
      : React.null}

    <BoardDrawer
      open_={drawerOpen}
      boards={boardList}
      activeId={board.id}
      onClose={() => setDrawerOpen(_ => false)}
      onNew={() => createBoard()->ignore}
      onSelect={id => switchBoard(id)->ignore}
      onDelete={(id, name) => deleteBoard(id, name)->ignore}
    />

    <AiPanel
      open_={aiOpen}
      board
      onClose={() => setAiOpen(_ => false)}
      onGenerated={(generated, modifying) => {
        let next = modifying
          ? {...generated, id: board.id, createdAt: board.createdAt}
          : {...generated, id: board.id, createdAt: board.createdAt}
        replaceBoard(next)
        setSelection(_ => NoSelection)
        setView(_ => defaultView)
        showToast(modifying ? "Diagram modified" : "Diagram generated")
      }}
    />

    <IconPalette
      open_={iconOpen}
      onClose={() => setIconOpen(_ => false)}
      onPick={(name, glyph) => {
        let shape = Model.makeShape(
          ~kind="icon",
          ~x=-.view.panX /. view.zoom -. 90.0,
          ~y=-.view.panY /. view.zoom -. 55.0,
          ~label=name,
          ~fill,
          ~layer=view.activeLayer,
          ~fontSize,
          ~icon=glyph,
          (),
        )
        replaceBoard(BoardOps.addShape(board, shape))
        setSelection(_ => ShapeSelection(shape.id))
        setIconOpen(_ => false)
        setTool(_ => "select")
      }}
    />

    {switch help {
    | Some(kind) => <HelpModal kind onClose={() => setHelp(_ => None)} />
    | None => React.null
    }}
    {switch shareLink {
    | Some(url) =>
      <div className="confirm-dialog is-open" onClick={_ => setShareLink(_ => None)}>
        <div
          className="confirm-dialog__card"
          role="dialog"
          ariaModal=true
          ariaLabel="Share board"
          onClick={event => event->ReactEvent.Mouse.stopPropagation}
        >
          <h2 className="confirm-dialog__title"> {React.string("Share board")} </h2>
          <p className="confirm-dialog__message">
            {React.string("Anyone with this link can open a compressed copy of this board.")}
          </p>
          <input className="ai-panel__input" value={url} readOnly=true ariaLabel="Share link" />
          <div className="confirm-dialog__actions">
            <button
              className="confirm-dialog__btn confirm-dialog__btn--ghost"
              type_="button"
              onClick={_ => setShareLink(_ => None)}
            >
              {React.string("Close")}
            </button>
            <button
              className="confirm-dialog__btn confirm-dialog__btn--primary"
              type_="button"
              onClick={_ => {
                let copy = async () => {
                  let copied = await Browser.copy(url)
                  showToast(copied ? "Share link copied" : "Select the link and copy it")
                }
                copy()->ignore
              }}
            >
              {React.string("Copy share link")}
            </button>
          </div>
        </div>
      </div>
    | None => React.null
    }}
    <div className={toast == "" ? "toast" : "toast is-visible"}> {React.string(toast)} </div>
    <div className="tabletop-vignette" />
  </div>
}
