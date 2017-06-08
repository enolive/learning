import {expect} from "chai";

export class Engine {

    nextState(current: CellState, livingNeighbours: number) {
        return CellState.Dead;
    }
}

export enum CellState {
    Dead,
    Living

}

describe("Rules of Game of Life", () => {
   it("should cell with less than 2 living neighbours die", () => {
       const engine = new Engine();
       const state = CellState.Living;
       const livingNeighbours = 1;
       const nextState = engine.nextState(state, livingNeighbours);
       expect(nextState).to.equal(CellState.Dead);
   })
});