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

    it("should accept multi-digit input for lookup", () => {
        const digit = new RomanDigit("II");
        expect(digit.toArabic()).to.equal(1);
    });

    it("should detect higher value of next digit", () => {
        const digit = new RomanDigit("IV");
        expect(digit.isFollowingHigher()).to.equal(true);
    });

    it("should detect lower value of next digit", () => {
        const digit = new RomanDigit("VI");
        expect(digit.isFollowingHigher()).to.equal(false);
    });

    it("should ignore missing follower correctly", () => {
        const digit = new RomanDigit("V");
        expect(digit.isFollowingHigher()).to.equal(false);
    });
});
