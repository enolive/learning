import {expect} from "chai";
import {RomanConverter} from "../src/roman.converter";

describe("Roman to Arabic converter", () => {
    let target: RomanConverter;

    beforeEach(() => target = new RomanConverter());

    it("should convert single digits", () => {
        expect(target.toArabic("I")).to.equal(1);
        expect(target.toArabic("V")).to.equal(5);
        expect(target.toArabic("X")).to.equal(10);
        expect(target.toArabic("L")).to.equal(50);
    });

});
