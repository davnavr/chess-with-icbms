module private IcbmChess.Host

open Browser.Dom

open Elmish

let update msg game = invalidOp "bad"

do
    // TODO: Maybe call on body.onLoad
    Program.mkSimple
        Game.start
        update
        (invalidOp "bad")
    |> Program.run
