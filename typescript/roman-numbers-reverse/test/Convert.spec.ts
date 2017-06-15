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

        it("should return X for arabic less than 50", () => {
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
            expect(Convert.toRomanNumber(123)).to.equal("CXXIII"); 
            expect(Convert.toRomanNumber(359)).to.equal("CCCLIX"); 
        });
    });

    describe("subtraction rules", () => {
        it("should return IV for arabic 4", () => {
            expect(Convert.toRomanNumber(4)).to.equal("IV");
        });

        it("should return IX for arabic 9", () => {
            expect(Convert.toRomanNumber(9)).to.equal("IX");
        });
    });
});
