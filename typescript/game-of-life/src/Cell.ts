import {CellState} from "./CellState";
import {Position} from "./Position";

export class Cell {
    public get position(): Position {
        return this._position;
    }

    public get state(): CellState {
        return this._state;
    }

    private _state: CellState;
    private _position: Position;

    public constructor(state: CellState, position: Position) {
        this._state = state;
        this._position = position;
    }
}
