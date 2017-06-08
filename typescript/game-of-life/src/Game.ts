import {Board} from "./Board";
import {CellState} from "./CellState";
import {Dimensions} from "./Dimensions";
import {Position} from "./Position";
import {RuleEngine} from "./RuleEngine";
//noinspection TsLint
import * as _ from "lodash";

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

        const positions = Game.positions(this._dimensions);
        positions.forEach((position) => {
            const nextCell = this.nextStateOf(position);
            if (nextCell === CellState.Living) {
                newBoard.setCellsAliveAt(position);
            } else {
                newBoard.setCellsDeadAt(position);
            }
        });

        this._board = newBoard;
    }

    private static positions(dimensions: Dimensions) {
        const positions = [];
        for (let x = 0; x < dimensions.width; x++) {
            for (let y = 0; y < dimensions.height; y++) {
                const position = new Position(x, y);
                positions.push(position);
            }
        }
        return positions;
    }

    private nextStateOf(position: Position) {
        const livingNeighbours = this._board.countLivingNeighboursOf(position);
        const currentCell = this._board.isCellAliveAt(position)
            ? CellState.Living : CellState.Dead;
        const nextCell = RuleEngine.nextState(currentCell, livingNeighbours);
        return nextCell;
    }
}
