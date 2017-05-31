import {expect} from "chai";
import {AllDigits} from "../src/all.digits";
//noinspection TsLint - ordering this into the other imports seems not possible
import * as _ from "lodash";

describe("All digits", () => {
    it("should return expected digit when followed by lower value", () => {
        expect(valueOfFirstDigit("VIII")).to.equal(5);
    });

    it("should return expected digit when followed by higher value", () => {
        expect(valueOfFirstDigit("IX")).to.equal(-1);
    });

    it("should return expected last digit", () => {
        expect(valueOfLastDigit("XVI")).to.equal(1);
    });

    function valueOfFirstDigit(roman: string) {
        return _.head(AllDigits.of(roman)).toArabic();
    }

    function valueOfLastDigit(roman: string) {
        return _.last(AllDigits.of(roman)).toArabic();
    }
});