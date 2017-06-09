import {expect} from "chai";
import {Dimensions} from "../src/Dimensions";
import {Position} from "../src/Position";

describe("Dimensions of Board", () => {
    it("should have zero positions for 0x0", () => {
        expect(new Dimensions(0, 0).positions()).to.be.empty;
        expect(new Dimensions(-100, -42).positions()).to.be.empty;
    });

    it("should have exactly one position for 1x1", () => {
        const values = valuesFrom(new Dimensions(1, 1).positions());
        expect(values).to.deep.equal([[0, 0]]);
    });

    it("should have four positions for 2x2", () => {
        const values = valuesFrom(new Dimensions(2, 2).positions());
        expect(values).to.deep.equal([[0, 0], [0, 1], [1, 0], [1, 1]]);
    });

    it("should have six positions for 3x2", () => {
        const values = valuesFrom(new Dimensions(3, 2).positions());
        expect(values).to.deep.equal([[0, 0], [0, 1], [1, 0], [1, 1], [2, 0], [2, 1]]);
    });

    function valuesFrom(positions: Position[]) {
        return positions.map((p) => [p.x, p.y]);
    }
});
