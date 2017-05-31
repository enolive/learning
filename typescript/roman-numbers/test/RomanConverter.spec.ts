import {expect} from "chai";
import {ConvertRoman} from "../src/RomanConverter";

describe("Roman to Arabic converter", () => {

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

    describe("subtraction of digits", () => {
        it("should return 9 for IX", () => {
            expect(ConvertRoman.toArabic("IX")).to.equal(9);
        });

        it("should return 42 for XLII", () => {
            expect(ConvertRoman.toArabic("XLII")).to.equal(42);
        });
    });

    describe("high-level tests", () => {
        it("should return 1984", () => {
            expect(ConvertRoman.toArabic("MCMLXXXIV")).to.equal(1984);
        });
    });
});
