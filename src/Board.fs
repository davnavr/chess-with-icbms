namespace IcbmChess

type Player =
    | White
    | Black

    member this.Opponent =
        match this with
        | White -> Black
        | Black -> White

[<StructuralComparison; StructuralEquality>]
type File =
    | A
    | B
    | C
    | D
    | E
    | F
    | G
    | H

type Rank =
    | Rank of uint8

    override this.ToString() =
        let (Rank rank) = this
        int rank + 1 |> sprintf "%i"

module Rank =
    /// <exception cref="T:System.ArgumentException">Thrown when the rank is not between 0 and 7.</exception>
    let create rank =
        if rank < 8uy
        then Rank rank
        else
            invalidArg "rank" "Rank must be between 0 and 7."

[<StructuralComparison; StructuralEquality>]
type Coord =
    | Coord of File * Rank

    override this.ToString() =
        let (Coord(file, rank)) = this
        let file' = file.ToString().ToLower()
        sprintf "%s%O" file' rank

type PieceType =
    | King
    | Queen
    | Rook
    | Bishop
    | Knight
    | Pawn

type Piece =
    { Nukes: uint32
      Owner: Player
      Type: PieceType }

    override this.ToString() =
        let owner =
            match this.Owner with
            | White -> 0
            | Black -> 6
        let piece =
            let code =
                match this.Type with
                | King -> 0x2654
                | Queen -> 0x2655
                | Rook -> 0x2656
                | Bishop -> 0x2657
                | Knight -> 0x2658
                | Pawn -> 0x2659
            code + owner |> sprintf "&#x%X"
        let nuke =
            match this.Nukes with
            | 0u -> ""
            | _ -> sprintf "&#x1F680 %i" this.Nukes
        sprintf "%s%s" piece nuke

[<RequireQualifiedAccess>]
module Board =
    type Info =
        private
        | Board of Map<File, Piece option>[]

    let init =
        let files = [ A; B; C; D; E; F; G; H ]
        let emptyFile =
            files
            |> List.map (fun file -> file, None)
            |> Map.ofList
        let rank owner =
            List.map
                (fun ptype ->
                    { Nukes = 0u
                      Owner = owner
                      Type = ptype }
                    |> Some)
            >> List.zip files
            >> Map.ofList
        let start owner =
            [
                Rook
                Knight
                Bishop
                Queen
                King
                Bishop
                Knight
                Rook
            ]
            |> rank owner
        let pawns owner =
            List.replicate 8 Pawn |> rank owner
        fun() ->
            Array.init
                8
                (function
                | 7 -> start Black
                | 6 -> pawns Black
                | 0 -> start White
                | 1 -> pawns White
                | _ -> emptyFile)
            |> Board

    let getPiece (Coord(file, Rank rank)) (Board board) =
        board
        |> Array.item (int rank)
        |> Map.find file

    let toSeq (Board board) =
        board
        |> Seq.ofArray
        |> Seq.mapi
            (fun rank files ->
                let rank' = uint8 rank |> Rank
                let files' = Map.toList files
                List.map
                    (fun (file, piece) -> Coord(file, rank'), piece)
                    files')
        |> Seq.rev

    let pieces =
        let files =
            List.choose
                (fun (pos, piece) ->
                    Option.map
                        (fun piece' -> pos, piece')
                        piece)
        toSeq
        >> Seq.map files
        >> Seq.collect id
