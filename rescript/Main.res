open Model

let resolveStoredBoard = (): board => {
  let boards = Persistence.loadBoardsSync()
  let requestedId =
    Browser.hash->String.startsWith("#/board/") ? Some(Browser.hash->String.slice(~start=8)) : None
  let selected = switch requestedId {
  | Some(id) => boards->Array.find(board => board.id == id)
  | None => boards->Array.get(0)
  }
  let board = switch selected {
  | Some(board) => Model.upgradeLegacyStarter(board)
  | None =>
    let starter = Model.starterBoard()
    Persistence.saveBoardsSync([starter])
    starter
  }
  Browser.navigate("#/board/" ++ board.id)
  board
}

let (board, shared) = switch Browser.searchParam("g") {
| Some(graph) =>
  switch Ai.boardFromGeneratedGraphJson(graph) {
  | Some(board) => (board, true)
  | None => (resolveStoredBoard(), false)
  }
| None =>
  switch Browser.searchParam("b") {
  | Some(code) =>
    switch Share.decodeBoard(code) {
    | Some(board) => (board, true)
    | None => (resolveStoredBoard(), false)
    }
  | None => (resolveStoredBoard(), false)
  }
}

switch ReactDOM.querySelector("#app") {
| Some(rootElement) =>
  rootElement
  ->ReactDOM.Client.createRoot
  ->ReactDOM.Client.Root.render(<App initialBoard={board} initialShared={shared} />)
| None => Console.error("Sketch Lab could not find the #app mount point.")
}
