import {expect} from "chai";
import {Generator} from "../src/generator";

describe("Fizz Buzz Generator", () => {
    let target: Generator;

    beforeEach(() => target = new Generator());

    it("should return normal number as is", () => {
        expect(target.resultFor(1)).to.equal("1");
        expect(target.resultFor(2)).to.equal("2");
    });

    it("should return Fizz for numbers that are divisible by 3", () => {
        expect(target.resultFor(3)).to.equal("Fizz");
        expect(target.resultFor(6)).to.equal("Fizz");
    });

    it("should return Buzz for numbers that are divisible by 5", () => {
        expect(target.resultFor(5)).to.equal("Buzz");
        expect(target.resultFor(10)).to.equal("Buzz");
    });
});
