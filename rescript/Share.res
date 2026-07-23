open Model

type lzString

@module("lz-string")
external lzString: lzString = "default"

@send
external compressValue: (lzString, string) => string = "compressToEncodedURIComponent"

@send @return(nullable)
external decompressValue: (lzString, string) => option<string> = "decompressFromEncodedURIComponent"

external boardToJson: board => JSON.t = "%identity"
external boardFromJson: JSON.t => board = "%identity"

let encodeBoard = (board: board): string =>
  lzString->compressValue(board->boardToJson->JSON.stringify)

let isValidBoard = (board: board): bool =>
  board.id != "" &&
  board.name != "" &&
  board.shapes->Array.every(shape => shape.id != "" && shape.kind != "") &&
  board.edges->Array.every(edge => edge.id != "")

let decodeBoard = (encoded: string): option<board> =>
  switch lzString->decompressValue(encoded) {
  | None => None
  | Some(text) =>
    try {
      let board = text->JSON.parseOrThrow->boardFromJson
      isValidBoard(board) ? Some(board) : None
    } catch {
    | _ => None
    }
  }
