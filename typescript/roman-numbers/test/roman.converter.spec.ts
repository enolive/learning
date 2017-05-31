import {expect} from "chai";
import {RomanConverter} from "../src/roman.converter";

describe("Roman to Arabic converter", () => {
    let target: RomanConverter;

    beforeEach(() => target = new RomanConverter());

    describe("single digits", () => {
        it("should be converted", () => {
            expect(target.toArabic("I")).to.equal(1);
            expect(target.toArabic("V")).to.equal(5);
            expect(target.toArabic("X")).to.equal(10);
            expect(target.toArabic("L")).to.equal(50);
            expect(target.toArabic("C")).to.equal(100);
            expect(target.toArabic("D")).to.equal(500);
            expect(target.toArabic("M")).to.equal(1000);
        });

        it("should throw on invalid", () => {
            expect(() => target.toArabic("R")).to.throw(RangeError, "Invalid roman digit 'R'.");
        });
    });
});
