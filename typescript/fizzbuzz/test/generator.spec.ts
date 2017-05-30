import {Generator} from '../src/generator';

describe('Fizz Buzz Generator', () => {
    let target: Generator;

    beforeEach(() => {
        target = new Generator();
    });

    it('should return normal numbers', () => {
        expect(target.resultFor(1)).toEqual('1');
        expect(target.resultFor(2)).toEqual('2');
    });

    it('should return Fizz for numbers divisible by 3', () => {
        expect(target.resultFor(3)).toEqual('Fizz');
        expect(target.resultFor(6)).toEqual('Fizz');
    });

    it('should return Buzz for numbers divisible by 5', () => {
        expect(target.resultFor(5)).toEqual('Buzz');
        expect(target.resultFor(10)).toEqual('Buzz');
    });

    it('should return Fizz-Buzz for numbers divisible by 3 and 5', () => {
        expect(target.resultFor(15)).toEqual('Fizz-Buzz');
        expect(target.resultFor(30)).toEqual('Fizz-Buzz');
    });

});