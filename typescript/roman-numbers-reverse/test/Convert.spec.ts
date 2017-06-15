import {expect} from "chai";
import {Convert} from "../src/Convert";

describe("convert arabic to roman", () => {
    describe("addition rules", () => {
        it("should return I for arabic less than 4", () => {
            expect(Convert.toRomanNumber(1)).to.be.equal("I");
            expect(Convert.toRomanNumber(2)).to.be.equal("II");
            expect(Convert.toRomanNumber(3)).to.be.equal("III");
        });

        it("should return V for arabic less than 9", () => {
            expect(Convert.toRomanNumber(5)).to.equal("V");
            expect(Convert.toRomanNumber(6)).to.equal("VI");
            expect(Convert.toRomanNumber(8)).to.equal("VIII");
        });

        it("should return X for arabic less than 40", () => {
            expect(Convert.toRomanNumber(10)).to.equal("X");
            expect(Convert.toRomanNumber(15)).to.equal("XV");
            expect(Convert.toRomanNumber(18)).to.equal("XVIII");
        });

        it("should return L for arabic less than 90", () => {
            expect(Convert.toRomanNumber(50)).to.equal("L");
            expect(Convert.toRomanNumber(52)).to.equal("LII");
            expect(Convert.toRomanNumber(75)).to.equal("LXXV");
        });

        it("should return C for arabic less than 400", () => {
            expect(Convert.toRomanNumber(100)).to.equal("C");
            expect(Convert.toRomanNumber(359)).to.equal("CCCLIX");
        });

        it("should return D for arabic less than 900", () => {
            expect(Convert.toRomanNumber(500)).to.equal("D");
            expect(Convert.toRomanNumber(871)).to.equal("DCCCLXXI");
        });

        it("should return M for arabic above 1000", () => {
            expect(Convert.toRomanNumber(1000)).to.equal("M");
            expect(Convert.toRomanNumber(1458)).to.equal("MCDLVIII");
        });
    });

    describe("subtraction rules", () => {
        it("should return IV for arabic 4", () => {
            expect(Convert.toRomanNumber(4)).to.equal("IV");
        });

        it("should return IX for arabic 9", () => {
            expect(Convert.toRomanNumber(9)).to.equal("IX");
        });

        it("should return XL for arabic 40", () => {
            expect(Convert.toRomanNumber(40)).to.equal("XL");
        });

        it("should return XC for arabic 90", () => {
            expect(Convert.toRomanNumber(90)).to.equal("XC");
        });

        it("should return CD for arabic 400", () => {
            expect(Convert.toRomanNumber(400)).to.equal("CD");
        });

        it("should return CM for arabic 900", () => {
            expect(Convert.toRomanNumber(900)).to.equal("CM");
        });
    });

    describe("high-level tests", () => {
        it("should convert 1984", () => {
            expect(Convert.toRomanNumber(1984)).to.equal("MCMLXXXIV");
        });
    });
});
