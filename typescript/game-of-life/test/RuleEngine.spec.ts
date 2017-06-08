import {expect} from "chai";
import {CellState} from "../src/CellState";
import {RuleEngine} from "../src/RuleEngine";

describe("Rules of Game of Life", () => {
    it("should let cell with less than 2 living neighbours die", () => {
        expect(RuleEngine.nextState(CellState.Living, 0)).to.equal(CellState.Dead);
        expect(RuleEngine.nextState(CellState.Living, 1)).to.equal(CellState.Dead);
    });

    it("should let cell with 2 or 3 living neighbours survive", () => {
        expect(RuleEngine.nextState(CellState.Living, 2)).to.equal(CellState.Living);
        expect(RuleEngine.nextState(CellState.Living, 3)).to.equal(CellState.Living);
    });

    it("should let cell with 2 living neighbours stay dead", () => {
        expect(RuleEngine.nextState(CellState.Dead, 2)).to.equal(CellState.Dead);
    });

    it("should let cell with more than 3 living neighbours die", () => {
        expect(RuleEngine.nextState(CellState.Living, 4)).to.equal(CellState.Dead);
        expect(RuleEngine.nextState(CellState.Living, 8)).to.equal(CellState.Dead);
    });

    it("should let cell with exactly 3 neighbours be born", () => {
        expect(RuleEngine.nextState(CellState.Dead, 3)).to.equal(CellState.Living);
    });
});
