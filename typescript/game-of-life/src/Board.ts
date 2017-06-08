import {Position} from "./Position";

export class Board {
    private wasSetAlive: boolean = false;

    public isCellAliveAt(location: Position) {
        return this.wasSetAlive;
    }

    public setCellAliveAt(position: Position) {
        this.wasSetAlive = true;
    }
}
