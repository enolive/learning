import {Set} from "hash-set-map";
import {Position} from "./Position";

export class Board {

    private livingCells = new Set(Position.hash);

    public isCellAliveAt(position: Position) {
        return this.livingCells.has(position);
    }

    public setCellAliveAt(position: Position) {
        this.livingCells.add(position);
    }
}
