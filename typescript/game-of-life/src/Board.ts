import {Set} from "hash-set-map";
import {Position} from "./Position";

export class Board {

    private livingCells = new Set<Position>(Position.hash);

    public isCellAliveAt(position: Position) {
        return this.livingCells.has(position);
    }

    public setCellsAliveAt(...position: Position[]) {
        position.forEach((p) => this.livingCells.add(p));
    }
}
