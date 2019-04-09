module Life exposing (..)

import Debug exposing (todo)

type alias Cell = {x : Int, y : Int}

type CellStatus = Dead | Alive

type alias Board = Cell -> CellStatus

sampleBoard : Board
sampleBoard cell =
    case (cell.x, cell.y) of
        (1, 0) -> Alive
        (1, 1) -> Dead
        (1, 2) -> Alive
        (2, 0) -> Alive
        (2, 1) -> Alive
        (2, 2) -> Dead
        (0, 0) -> Dead
        (0, 1) -> Alive
        (0, 2) -> Dead
        _ -> Alive


nextStatus : Int -> CellStatus -> CellStatus
nextStatus numLivingNeighbors currStatus = 
    if ((numLivingNeighbors < 2) && currStatus == Alive)
        then Dead
        else if ((numLivingNeighbors > 3) && currStatus == Alive)
            then Dead
            else if ((numLivingNeighbors == 3) && currStatus == Dead)
                then Alive
                else currStatus


livingNeighbors : Board -> Cell -> Int
livingNeighbors currBoard {x, y} =
    let
        listOfNeighbors : List Cell
        listOfNeighbors = [
                {x = x + 1, y = y},
                {x = x + 1, y = y + 1},
                {x = x + 1, y = y - 1},
                {x = x, y = y + 1},
                {x = x, y = y - 1},
                {x = x - 1, y = y + 1},
                {x = x - 1, y = y},
                {x = x - 1, y = y - 1}
                ]   

        cellStatusToInt : CellStatus -> Int
        cellStatusToInt status =
            case status of
                Alive -> 1
                Dead -> 0

        listOfCellStatus : List CellStatus
        listOfCellStatus = List.map (currBoard) (listOfNeighbors)

        listOfLive : List Int
        listOfLive = List.map (cellStatusToInt) (listOfCellStatus)

        sumOfList : Int
        sumOfList = List.sum listOfLive
    in
    sumOfList
    

nextBoard : Board -> Board
nextBoard currBoard cell =
    let
        aliveCells = livingNeighbors currBoard cell
        nextCellStatus = nextStatus aliveCells (currBoard cell)
            
    in
    nextCellStatus