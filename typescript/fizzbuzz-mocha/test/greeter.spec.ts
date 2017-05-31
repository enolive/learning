import {expect} from 'chai';
import {Greeter} from '../src/greeter';

describe('Greeter', () => {
    let target: Greeter;

    beforeEach(() => {
        target = new Greeter();
    });

    it('should greet the world', () => {
        let result = target.sayHello();
        expect(result).to.equal('Hello World!');
    });

    it('should greet the given person', () => {
       expect(target.sayHelloTo('Christoph')).to.equal('Hello Christoph!');
       expect(target.sayHelloTo('Tofu')).to.equal('Hello Tofu!');
    });
});