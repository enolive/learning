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

    public countLivingNeighboursOf(position: Position) {
        const candidates = new Position(position.x - 1, position.y - 1);
        if (this.isCellAliveAt(candidates)) {
            return 1;
        }
        return 0;
    }
}
