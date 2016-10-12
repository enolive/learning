///<reference path="../typing/jasmine.d.ts"/>
import {Greeter} from "../src/greeter";

describe("Greeter", () => {
    it("should greet the world", () => {
        // act
        let result = Greeter.greet();
        // assert
        expect(result).toBe("Hello, World!");
    });

    it("should greet a person", () => {
        // act
        let result = Greeter.greet("Christoph");
        // assert
        expect(result).toBe("Hello, Christoph!");
    });
});
