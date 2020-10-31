module private IcbmChess.Host

open Browser.Dom

open Elmish

let init() =
    let result, game = Game.start()
    game, Cmd.ofMsg result

let update (msg: MoveResult) game =
    match msg with
    | Ended _ -> game, Cmd.none
    | _ -> invalidOp "bad"

let view game (disp: Dispatch<MoveResult>) =
    ()

do
    // TODO: Maybe call on body.onLoad
    Program.mkProgram
        init
        update
        view
    |> Program.run
