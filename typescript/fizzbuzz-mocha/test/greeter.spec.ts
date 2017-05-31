import {expect} from 'chai';

class Greeter {
    sayHello(): string {
        return 'Hello World!';
    }
}

let sayHello = function () {
    let greeter = new Greeter();
    return greeter.sayHello();
};

describe('Greeter', () => {
    it('should greet the world', () => {
        let result = sayHello();
        expect(result).to.equal('Hello World!');
    });
});