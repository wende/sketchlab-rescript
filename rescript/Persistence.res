open Model

type storage

let storageKey = "sketchlab:rescript:boards"

@val @scope("window")
external localStorage: storage = "localStorage"

@send @return(nullable)
external getItem: (storage, string) => option<string> = "getItem"

@send
external setItem: (storage, string, string) => unit = "setItem"

external boardsToJson: array<board> => JSON.t = "%identity"
external boardsFromJson: JSON.t => array<board> = "%identity"

let loadBoardsSync = (): array<board> =>
  try {
    switch localStorage->getItem(storageKey) {
    | None => []
    | Some(value) => value->JSON.parseOrThrow->boardsFromJson
    }
  } catch {
  | _ => []
  }

let saveBoardsSync = (boards: array<board>): unit =>
  try {
    localStorage->setItem(storageKey, boards->boardsToJson->JSON.stringify)
  } catch {
  | _ => ()
  }

let loadBoards = async (): array<board> => loadBoardsSync()

let saveBoards = async (boards: array<board>): unit => saveBoardsSync(boards)

let saveBoard = async (board: board): unit => {
  let boards = loadBoardsSync()
  let exists = boards->Array.some(item => item.id == board.id)
  let next = exists
    ? boards->Array.map(item => item.id == board.id ? board : item)
    : boards->Array.concat([board])
  saveBoardsSync(next)
}

let deleteBoard = async (id: string): unit => {
  saveBoardsSync(loadBoardsSync()->Array.filter(board => board.id != id))
}

let findBoard = async (id: string): option<board> =>
  loadBoardsSync()->Array.find(board => board.id == id)

let boardMetas = async (): array<boardMeta> =>
  loadBoardsSync()
  ->Array.map(board => {
    id: board.id,
    name: board.name,
    updatedAt: board.updatedAt,
    shapeCount: board.shapes->Array.length,
  })
  ->Array.toSorted((a, b) => b.updatedAt -. a.updatedAt)
