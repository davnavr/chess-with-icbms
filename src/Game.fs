namespace rec IcbmChess

type Ended =
    | Won of Player
    | Tied

type Game =
    { Board: Board.Info
      Turn: uint32 }

type NextMove = unit -> MoveResult * Game

type MoveInfo =
    { Moves: Map<Coord, NextMove>
      Piece: PieceInfo }

type MoveResult =
    | Ended of Ended
    | PlayerToMove of
        {| Moves: Map<Coord, MoveInfo>
           Resign: NextMove
           Player: Player |}

[<RequireQualifiedAccess>]
module Game =
    let private resign (resigner: Player) state () =
        Won resigner.Opponent |> Ended, state

    let private moves player state =
        state.Board
        |> Board.pieces
        |> Seq.choose
            (function
            | pos, piece when piece.Owner = player ->
                match piece.Type with
                // TODO: Get all valid moves for moving a piece
                | _ -> []
                |> Some
            | _ -> None)
        |> Seq.collect id

    let private playerMove player state =
        let available = moves player state
        {| Moves = Map.empty // TODO: Put all available moves in this map.
           Resign = resign player state
           Player = player |}
        |> PlayerToMove,
        state

    let start(): MoveResult * Game =
        let state =
            { Board = Board.init()
              Turn = 0u }
        playerMove White state
