import {ConvertRoman} from "../src/roman.converter";
import {expect} from "chai";

describe("Single Roman Digit", () => {
    it("should be converted", () => {
        expect(ConvertRoman.toArabic("I")).to.equal(1);
        expect(ConvertRoman.toArabic("V")).to.equal(5);
        expect(ConvertRoman.toArabic("X")).to.equal(10);
        expect(ConvertRoman.toArabic("L")).to.equal(50);
        expect(ConvertRoman.toArabic("C")).to.equal(100);
        expect(ConvertRoman.toArabic("D")).to.equal(500);
        expect(ConvertRoman.toArabic("M")).to.equal(1000);
    });

    it("should throw on invalid", () => {
        expect(() => ConvertRoman.toArabic("R")).to.throw(RangeError, "Invalid roman digit 'R'.");
    });
});
