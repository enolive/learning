import {expect} from 'chai';
import {Generator} from '../src/generator';

describe('Fizz Buzz Generator', () => {
    let target: Generator;

    beforeEach(() => target = new Generator());

    it('should return normal number as is', () => {
        expect(target.resultFor(1)).to.equal('1');
        expect(target.resultFor(2)).to.equal('2');
    });
});