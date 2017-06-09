import {Board} from "./Board";
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
            .forEach((position) => {
                if (this.nextStateOf(position) === CellState.Living) {
                    newBoard.setCellsAliveAt(position);
                } else {
                    newBoard.setCellsDeadAt(position);
                }
            });

        this._board = newBoard;
    }

    private nextStateOf(position: Position) {
        const livingNeighbours = this._board.countLivingNeighboursOf(position);
        const currentCell = this._board.isCellAliveAt(position)
            ? CellState.Living : CellState.Dead;
        return RuleEngine.nextState(currentCell, livingNeighbours);
    }
}
