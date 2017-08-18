class Greeter {
    public sayHello() {
        return "Hello, World!";
    }
}

describe("Some test", () => {
    it("should succeed", () => {
        expect(true).toBeFalsy();
    });

    it("should greet the world", () => {
        const greeter = new Greeter();
        expect(greeter.sayHello()).toEqual("Hello, World!");
    });
});
