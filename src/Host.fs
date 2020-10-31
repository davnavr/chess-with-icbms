module private IcbmChess.Host

open Browser
open Browser.Types

open Elmish

let init() =
    let result, game = Game.start()
    game, Cmd.ofMsg result

let update (msg: MoveResult) game =
    match msg with
    | Ended _ -> game, Cmd.none
    | _ -> game, Cmd.none // TODO: Process player move.

let view (game: Game) (disp: Dispatch<MoveResult>) =
    let board = Board.toSeq game.Board
    let boardElem = document.body.querySelector "table.board"

    boardElem.innerHTML <- "" // TODO: Find better way to remove all child nodes.

    for files in board do // TODO: Figure out why the grid is duplicated twice.
        let fileElem =
            document.createElement("tr")
            |> boardElem.appendChild
            :?> Element

        fileElem.classList.add "board__rank"

        for (pos, _) in files do
            let squareElem =
                document.createElement("td")
                |> fileElem.appendChild
                :?> Element
            let buttonElem =
                document.createElement("button")
                |> squareElem.appendChild
                :?> Element

            buttonElem.innerHTML <- string pos
            squareElem.classList.add "board__square"
    ()

do
    // TODO: Maybe call on body.onLoad
    Program.mkProgram
        init
        update
        view
    |> Program.run
