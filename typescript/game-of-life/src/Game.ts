import {Board} from "./Board";
import {Cell} from "./Cell";
import {Dimensions} from "./Dimensions";
import {Position} from "./Position";
import {RuleEngine} from "./RuleEngine";

export class Game {
    private _dimensions: Dimensions;
    private _board: Board = new Board();

    public constructor(dimensions: Dimensions) {
        this._dimensions = dimensions;
    }

    public nextGeneration(): void {
        const newBoard = new Board();
        const oldBoard = this._board;
        this._dimensions
            .positions()
            .map((position) => oldBoard.getCellAt(position))
            .map((current) => Game.nextCell(oldBoard, current))
            .forEach((next) => newBoard.transform(next));
        this._board = newBoard;
    }

    public getCellAt(position: Position) {
        return this._board.getCellAt(position);
    }

    public transform(cell: Cell) {
        this._board.transform(cell);
    }

    private static nextCell(board: Board, currentCell: Cell) {
        const livingNeighbours = board.countLivingNeighboursOf(currentCell.position);
        const nextState = RuleEngine.nextState(currentCell.state, livingNeighbours);
        return new Cell(nextState, currentCell.position);
    }
}
