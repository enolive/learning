"use strict";
///<reference path="../typing/jasmine.d.ts"/>
var greeter_1 = require("../src/greeter");
describe("Greeter", function () {
    it("should greet the world", function () {
        // act
        var result = greeter_1.Greeter.greet();
        // assert
        expect(result).toBe("Hello, World!");
    });
    it("should greet a person", function () {
        // act
        var result = greeter_1.Greeter.greet("Christoph");
        // assert
        expect(result).toBe("Hello, Christoph!");
    });
});
//# sourceMappingURL=greeter_test.js.map