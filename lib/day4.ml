type tile =
  { value : int
  ; marked : bool
  }

type board = { tiles : tile list }

type game =
  { draws : int list
  ; boards : board list
  }
