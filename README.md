# Sketch Lab

A fast, browser-only diagram tool written in ReScript. Draw and resize shapes,
connect them with lines or arrows, add architecture symbols and images, organize
work across stacked floors, and share a compressed copy in one URL.

## Stack

- **Language:** ReScript 12 with fully typed records, variants, and JS bindings.
- **UI:** ReScript React 0.15 on React 19.
- **Rendering:** a perspective-projected SVG scene with a 60° pinhole camera,
  converging finite floor grid, extruded token pedestals, elevated floors, and
  boundary-anchored glowing connectors.
- **Persistence:** local browser storage with typed ReScript serialization.
- **Sharing:** board JSON compressed by `lz-string` into a `?b=` URL.
- **Generated diagrams:** JSON graph imports through `?g=` and optional direct
  generation through the OpenAI Responses API.
- **Build:** ReScript compiler + Vite; production remains a static nginx site.

All application and test source lives in `rescript/*.res`. Files ending in
`.res.js` are generated build artifacts and are ignored by Git.

## Commands

```bash
npm install
npm run dev       # compile ReScript, then start http://localhost:5173
npm run dev:res   # run ReScript and Vite in watch mode
npm test          # compile and run the ReScript test suite
npm run build     # strict ReScript compile + production Vite bundle
npm run preview   # serve the production build
```

## Features

- Rectangle, circle, text, code, icon, and image objects.
- Drag to move; use the corner handle to resize.
- Lines and directed arrows connect shape centers and remain attached while moving.
- Double-click shapes and connectors to edit labels.
- Searchable palette with more than 80 architecture symbols.
- Image insertion through the toolbar or drag-and-drop.
- Board drawer with create, switch, delete, rename, autosave, and reload persistence.
- Compressed share URLs and GeneratedGraph `?g=` imports.
- Undo/redo and copy/cut/paste keyboard shortcuts.
- Floor add, rename, hide, delete, activate, and selection assignment controls.
- Auto-layout, fill colors, global text sizes, zoom, pan, and fit controls.
- Generate-new and modify-current modes backed by structured OpenAI output.

## Architecture

```text
rescript/
  Model.res          typed board, shape, edge, layer, and selection model
  BoardOps.res       pure board transformations and auto-layout
  BoardCanvas.res    projected SVG scene and pointer/drop interaction state machine
  Perspective.res    camera projection, inverse hit mapping, fit, and floor bounds
  App.res            ReScript React editor, panels, routing actions, and history
  Persistence.res    local board storage
  Share.res          compressed board links
  Ai.res             GeneratedGraph conversion and OpenAI integration
  Browser.res        narrow typed browser bindings
  Main.res           share/generated/local-board bootstrap
  TestSuite.res      executable ReScript model tests
  styles.css         complete application styling
```

## Agent skill

Agents can emit Sketch Lab GeneratedGraph JSON and open it with a `?g=` URL. The
skill file is served with the app:

`/skills/sketch-lab/SKILL.md`

## Shortcuts

`V` select · `T` text · `C` code · `R` rectangle · `O` circle · `L` connector ·
`A` arrow · `M` pan · `/` icon palette · `Delete` remove selection ·
`⌘/Ctrl+C/X/V` copy/cut/paste · `⌘/Ctrl+Z` undo ·
`⌘/Ctrl+Shift+Z` or `Ctrl+Y` redo.
