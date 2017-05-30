import {Generator} from './generator';

describe('Fizz Buzz Generator', () => {
    it('should return normal numbers', () => {
        let target = new Generator();
        expect(target.forNumber(1)).toEqual('1');
    });
});