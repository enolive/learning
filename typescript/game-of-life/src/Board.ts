import {Set} from "hash-set-map";
import {Position} from "./Position";

export class Board {

    private livingCells = new Set<Position>(Position.hash);

    public isCellAliveAt(position: Position) {
        return this.livingCells.has(position);
    }

    public setCellsAliveAt(...positions: Position[]) {
        positions.forEach((p) => this.livingCells.add(p));
    }

    public countLivingNeighboursOf(position: Position) {
        return Board
            .neighboursOf(position)
            .filter((c) => this.isCellAliveAt(c))
            .length;
    }

    public setCellsDeadAt(...positions: Position[]) {
        positions.forEach((p) => this.livingCells.delete(p));
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
}