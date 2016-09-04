describe("Fizz Buzz Game", ()=> {
    var fizzBuzzEngine;

    beforeEach(() => fizzBuzzEngine = new FizzBuzzEngine());

    it("should return normal numbers as is",
        () => {
            expect(fizzBuzzEngine.calculateNext(1)).toBe("1");
            expect(fizzBuzzEngine.calculateNext(2)).toBe("2");
        });

    it("should return Fizz on numbers divisible by 3",
        () => {
            expect(fizzBuzzEngine.calculateNext(3)).toBe("Fizz");
            expect(fizzBuzzEngine.calculateNext(6)).toBe("Fizz");
        });

    it("should return Buzz on numbers divisible by 5",
        () => {
            expect(fizzBuzzEngine.calculateNext(5)).toBe("Buzz");
            expect(fizzBuzzEngine.calculateNext(10)).toBe("Buzz");
        });

    it("should return Fizz-Buzz on numbers divisible by both 3 and 5",
        () => {
            expect(fizzBuzzEngine.calculateNext(15)).toBe("Fizz-Buzz");
            expect(fizzBuzzEngine.calculateNext(30)).toBe("Fizz-Buzz");
        });
});

