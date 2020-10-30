namespace rec IcbmChess

type Ended =
    | Won of Player
    | Tied

type Game =
    { Board: Board.Info
      Turn: uint32 }

type MoveInfo =
    { Game: Game
      Moves: Map<Coord, unit -> MoveResult>
      Piece: PieceInfo }

type MoveResult =
    | Ended of
        {| Game: Game
           Result: Ended |}
    | PlayerToMove of
        {| Game: Game
           Moves: Map<Coord, MoveInfo>
           Resign: unit -> MoveResult
           Player: Player |}

[<RequireQualifiedAccess>]
module Game =
    let private resign (resigner: Player) state () =
        {| Game = state
           Result = Won resigner.Opponent |}
        |> Ended

    let private moves player state =
        state.Board
        |> Board.pieces
        |> List.choose
            (function
            | pos, piece when piece.Owner = player ->
                match piece.Type with
                // TODO: Get all valid moves for moving a piece
                | _ -> []
                |> Some
            | _ -> None)
        |> List.collect id

    let private playerMove player state =
        let available = moves player state
        {| Game = state
           Moves = invalidOp "bad"
           Resign = resign player state
           Player = player |}
        |> PlayerToMove

    let start(): MoveResult =
        let state =
            { Board = Board.init()
              Turn = 0u }
        playerMove White state
