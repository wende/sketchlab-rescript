type clipboard
type window
type keyboardEvent
type inputTarget
type searchParams
type eventTarget

@val @return(nullable)
external prompt: (string, string) => option<string> = "prompt"

@val
external confirm: string => bool = "confirm"

@val @scope("navigator")
external clipboard: clipboard = "clipboard"

@val
external window: window = "window"

@send
external addKeyboardListener: (window, string, keyboardEvent => unit) => unit = "addEventListener"

@send
external removeKeyboardListener: (window, string, keyboardEvent => unit) => unit =
  "removeEventListener"

@get
external key: keyboardEvent => string = "key"

@get
external metaKey: keyboardEvent => bool = "metaKey"

@get
external ctrlKey: keyboardEvent => bool = "ctrlKey"

@get
external shiftKey: keyboardEvent => bool = "shiftKey"

@send
external preventDefault: keyboardEvent => unit = "preventDefault"

@get
external eventTarget: keyboardEvent => eventTarget = "target"

@get
external tagName: eventTarget => string = "tagName"

@get
external isContentEditable: eventTarget => bool = "isContentEditable"

let isTypingEvent = event => {
  let target = event->eventTarget
  let tag = target->tagName
  tag == "INPUT" || tag == "TEXTAREA" || target->isContentEditable
}

@send
external writeText: (clipboard, string) => promise<unit> = "writeText"

@val @scope("location")
external origin: string = "origin"

@val @scope("location")
external pathname: string = "pathname"

@val @scope("location")
external search: string = "search"

@val @scope("location")
external hash: string = "hash"

@val
external locationObject: {..} = "location"

@new
external makeSearchParams: string => searchParams = "URLSearchParams"

@send @return(nullable)
external getSearchParam: (searchParams, string) => option<string> = "get"

@get
external formTarget: ReactEvent.Form.t => inputTarget = "target"

@get
external targetValue: inputTarget => string = "value"

@get
external targetChecked: inputTarget => bool = "checked"

@val
external setTimeout: (unit => unit, int) => float = "setTimeout"

@val
external clearTimeout: float => unit = "clearTimeout"

let searchParam = name => makeSearchParams(search)->getSearchParam(name)

let navigate = nextHash => locationObject["hash"] = nextHash

let navigateBoard = id => locationObject["href"] = origin ++ pathname ++ "#/board/" ++ id

let shareUrl = board => origin ++ pathname ++ "?b=" ++ Share.encodeBoard(board)

let copy = async text =>
  try {
    await clipboard->writeText(text)
    true
  } catch {
  | _ => false
  }

let formatDate = timestamp => {
  let diff = Date.now() -. timestamp
  if diff < 60000.0 {
    "just now"
  } else if diff < 3600000.0 {
    Math.round(diff /. 60000.0)->Float.toInt->Int.toString ++ "m ago"
  } else if diff < 86400000.0 {
    Math.round(diff /. 3600000.0)->Float.toInt->Int.toString ++ "h ago"
  } else {
    timestamp->Date.fromTime->Date.toLocaleDateString
  }
}
