import {expect} from 'chai';
import {Greeter} from '../src/greeter';

describe('Greeter', () => {
    let greeter;

    beforeEach(() => greeter = new Greeter());

    it('should greet the world', () => {
        expect(greeter.sayHello()).to.equal('Hello, World!');
    });

    it('should greet the specified person', () => {
        expect(greeter.sayHello('Christoph')).to.equal('Hello, Christoph!');
    });
});