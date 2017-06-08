import {expect} from "chai";
import {Board} from "../src/Board";
import {Position} from "../src/Position";
import * as _ from "lodash";
import {CellState} from "../src/CellState";
import {RuleEngine} from "../src/RuleEngine";

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
        for (let x = 0; x < this._dimensions.width; x++) {
            for (let y = 0; y < this._dimensions.height; y++) {
                const position = new Position(x, y);
                const livingNeighbours = this._board.countLivingNeighboursOf(position);
                const currentCell = this._board.isCellAliveAt(position)
                    ? CellState.Living : CellState.Dead;
                const nextCell = RuleEngine.nextState(currentCell, livingNeighbours);
                if (nextCell === CellState.Living) {
                    newBoard.setCellsAliveAt(position);
                } else {
                    newBoard.setCellsDeadAt(position);
                }
            }
        }
        this._board = newBoard;
    }
}

export class Dimensions {
    public get height(): number {
        return this._height;
    }

    public get width(): number {
        return this._width;
    }

    private _width: number;
    private _height: number;

    public constructor(width: number, height: number) {
        this._width = width;
        this._height = height;
    }
}

describe("Game of Life", () => {
    it("should let | become H in next generation", () => {
        const dimensions = new Dimensions(10, 10);
        const game = new Game(dimensions);
        game.setCellsAliveAt(
            new Position(1, 0),
            new Position(1, 1),
            new Position(1, 2),
        );
        game.nextGeneration();
        expect(game.isCellAliveAt(new Position(0, 0))).to.be.false;
        expect(game.isCellAliveAt(new Position(1, 0))).to.be.false;
        expect(game.isCellAliveAt(new Position(2, 0))).to.be.false;
        expect(game.isCellAliveAt(new Position(0, 1))).to.be.true;
        expect(game.isCellAliveAt(new Position(1, 1))).to.be.true;
        expect(game.isCellAliveAt(new Position(2, 1))).to.be.true;
        expect(game.isCellAliveAt(new Position(0, 2))).to.be.false;
        expect(game.isCellAliveAt(new Position(1, 2))).to.be.false;
        expect(game.isCellAliveAt(new Position(2, 2))).to.be.false;
    });
});