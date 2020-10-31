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
        let (Coord(file, Rank rank)) = this
        let file' = file.ToString().ToLower()
        sprintf "%s%O" file' rank

[<RequireQualifiedAccess>]
type Piece =
    | King
    | Queen
    | Rook
    | Bishop
    | Knight
    | Pawn

type PieceType =
    | NormalPiece of Piece
    /// Can't move, but can launch and defend against ICBMs
    | NukePiece of Piece * nukes: uint32

type PieceInfo =
    { Owner: Player
      Type: PieceType }

[<RequireQualifiedAccess>]
module Board =
    type Info =
        private
        | Board of Map<File, PieceInfo option>[]

    let init =
        let files = [ A; B; C; D; E; F; G; H ]
        let emptyFile =
            files
            |> List.map (fun file -> file, None)
            |> Map.ofList
        let rank owner =
            List.map
                (fun ptype ->
                    { Owner = owner
                      Type = NormalPiece ptype }
                    |> Some)
            >> List.zip files
            >> Map.ofList
        let start owner =
            [
                Piece.Rook
                Piece.Knight
                Piece.Bishop
                Piece.Queen
                Piece.King
                Piece.Bishop
                Piece.Knight
                Piece.Rook
            ]
            |> rank owner
        let pawns owner =
            List.replicate 8 Piece.Pawn |> rank owner
        fun() ->
            Array.init
                8
                (function
                | 7 -> start Black
                | 6 -> pawns Black
                | 1 -> start White
                | 2 -> pawns White
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
