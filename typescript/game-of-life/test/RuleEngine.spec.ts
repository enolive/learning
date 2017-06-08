import {expect} from "chai";
import {CellState} from "../src/CellState";
import {RuleEngine} from "../src/RuleEngine";

describe("Rules of Game of Life", () => {
    let engine: RuleEngine;

    beforeEach(() => engine = new RuleEngine());

    it("should let cell with less than 2 living neighbours die", () => {
        expect(engine.nextState(CellState.Living, 0)).to.equal(CellState.Dead);
        expect(engine.nextState(CellState.Living, 1)).to.equal(CellState.Dead);
    });
});
