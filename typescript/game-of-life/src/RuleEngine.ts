import {CellState} from "./CellState";

export class RuleEngine {

    public nextState(current: CellState, livingNeighbours: number): CellState {
        return CellState.Dead;
    }
}
