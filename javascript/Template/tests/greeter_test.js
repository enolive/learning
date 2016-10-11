///<reference path="../src/greeter.ts"/>
///<reference path="../typing/jasmine.d.ts"/>
describe("Greeter", function () {
    it("should greet the world", function () {
        // act
        var result = Greeter.greet();
        // assert
        expect(result).toBe("Hello, World!");
    });
    it("should greet a person", function () {
        // act
        var result = Greeter.greet("Christoph");
        // assert
        expect(result).toBe("Hello, Christoph!");
    });
});
//# sourceMappingURL=greeter_test.js.map