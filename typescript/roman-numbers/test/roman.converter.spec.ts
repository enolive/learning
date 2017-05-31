import {expect} from "chai";
import {ConvertRoman} from "../src/roman.converter";

describe("Roman to Arabic converter", () => {
    describe("single digits", () => {
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

    describe("summation of digits", () => {
        it("should return 2 for II", () => {
           expect(ConvertRoman.toArabic("II")).to.equal(2);
        });

        it("should return 6 for VI", () => {
           expect(ConvertRoman.toArabic("VI")).to.equal(6);
        });

        it("should return 42 for XXXXII", () => {
           expect(ConvertRoman.toArabic("XXXXII")).to.equal(42);
        });
    });
});
