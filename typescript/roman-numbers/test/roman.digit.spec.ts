import {expect} from "chai";
import {RomanDigit} from "../src/roman.digit";

describe("Single Roman Digit", () => {
    it("should be converted", () => {
        expect(new RomanDigit("I").toArabic()).to.equal(1);
        expect(new RomanDigit("V").toArabic()).to.equal(5);
        expect(new RomanDigit("X").toArabic()).to.equal(10);
        expect(new RomanDigit("L").toArabic()).to.equal(50);
        expect(new RomanDigit("C").toArabic()).to.equal(100);
        expect(new RomanDigit("D").toArabic()).to.equal(500);
        expect(new RomanDigit("M").toArabic()).to.equal(1000);
    });

    it("should throw on invalid", () => {
        expect(() => new RomanDigit("R").toArabic())
            .to.throw(RangeError, "Invalid roman digit 'R'.");
    });
});
