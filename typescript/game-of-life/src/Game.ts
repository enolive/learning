import {Board} from "./Board";
import {Cell} from "./Cell";
import {CellState} from "./CellState";
import {Dimensions} from "./Dimensions";
import {Position} from "./Position";
import {RuleEngine} from "./RuleEngine";

export class Game {
    private _dimensions: Dimensions;
    private _board: Board = new Board();

    public constructor(dimensions: Dimensions) {
        this._dimensions = dimensions;
    }

    public setCellsAliveAt(...positions: Position[]): void {
        this._board.setCellsAliveAt(...positions);
    }

    public isCellAliveAt(position: Position): boolean {
        return this._board.isCellAliveAt(position);
    }

    public nextGeneration(): void {
        const newBoard = new Board();
        this._dimensions
            .positions()
            .map((position) => Game.currentCell(this._board, position))
            .map((current) => Game.nextCell(this._board, current))
            .forEach((next) => Game.transform(newBoard, next));
        this._board = newBoard;
    }

    private static transform(newBoard: Board, cell: Cell) {
        if (cell.state === CellState.Living) {
            newBoard.setCellsAliveAt(cell.position);
        } else {
            newBoard.setCellsDeadAt(cell.position);
        }
    }

    private static nextCell(board: Board, currentCell: Cell) {
        const livingNeighbours = board.countLivingNeighboursOf(currentCell.position);
        const nextState = RuleEngine.nextState(currentCell.state, livingNeighbours);
        return new Cell(nextState, currentCell.position);
    }

    private static currentCell(board: Board, position: Position) {
        const currentState = board.isCellAliveAt(position)
            ? CellState.Living : CellState.Dead;
        return new Cell(currentState, position);
    }
}
