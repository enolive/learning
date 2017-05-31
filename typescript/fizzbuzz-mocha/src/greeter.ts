export class Greeter {
    sayHello(): string {
        return this.sayHelloTo('World');
    }

    sayHelloTo(name: string): string {
        return `Hello ${name}!`;
    }
}