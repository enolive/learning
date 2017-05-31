import {expect} from "chai";
import {ConvertRoman} from "../src/roman.converter";

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
});
