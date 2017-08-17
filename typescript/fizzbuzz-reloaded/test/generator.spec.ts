import {expect} from "chai";
import {Generator} from "../src/generator";

describe("Fizz-Buzz Generator", () => {
    let generator: Generator;

    beforeEach(() => generator = new Generator());

    it("should return normal numbers as is", () => {
        expect(generator.calculate(1)).to.equal("1");
        expect(generator.calculate(2)).to.equal("2");
    });

    it("should return Fizz for numbers divisible by 3", () => {
        expect(generator.calculate(3)).to.equal("Fizz");
        expect(generator.calculate(6)).to.equal("Fizz");
    });

    it("should return Buzz for numbers divisible by 5", () => {
        expect(generator.calculate(5)).to.equal("Buzz");
        expect(generator.calculate(10)).to.equal("Buzz");
    });

    it("should return Fizz-Buzz for numbers divisible by 3 and 5", () => {
        expect(generator.calculate(15)).to.equal("Fizz-Buzz");
        expect(generator.calculate(45)).to.equal("Fizz-Buzz");
    });

});
