export class Greeter {
    static greet(name?: string): string {
        name = name || "World";
        return `Hello, ${name}!`;
    }
}