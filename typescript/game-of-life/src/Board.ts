import {Set} from "hash-set-map";
import {Cell} from "./Cell";
import {CellState} from "./CellState";
import {Position} from "./Position";

export class Board {

    private livingCells = new Set<Position>(Position.hashKey);

    public countLivingNeighboursOf(position: Position) {
        return Board
            .neighboursOf(position)
            .filter((c) => this.isCellAliveAt(c))
            .length;
    }

    public getCellAt(position: Position): Cell {
        const state = this.isCellAliveAt(position)
            ? CellState.Living
            : CellState.Dead;
        return new Cell(state, position);
    }

    public transform(cell: Cell) {
        if (cell.state === CellState.Living) {
            this.livingCells.add(cell.position);
        } else {
            this.livingCells.delete(cell.position);
        }
    }

    private static neighboursOf(position: Position) {
        return [
            new Position(position.x - 1, position.y - 1),
            new Position(position.x, position.y - 1),
            new Position(position.x + 1, position.y - 1),
            new Position(position.x - 1, position.y),
            new Position(position.x + 1, position.y),
            new Position(position.x - 1, position.y + 1),
            new Position(position.x, position.y + 1),
            new Position(position.x + 1, position.y + 1),
        ];
    }

    private isCellAliveAt(position: Position) {
        return this.livingCells.has(position);
    }
}
