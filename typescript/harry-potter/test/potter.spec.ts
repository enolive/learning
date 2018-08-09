import {expect} from 'chai';

function priceBundle(bundleSize: number) {
    return [8, 15.2, 21.6, 25.6, 30][bundleSize - 1];
}

describe('Harry Potter Kata', () => {
    describe('bundle price', () => {
        [
            {size: 1, price: 8},
            {size: 2, price: 15.2},
            {size: 3, price: 21.6},
            {size: 4, price: 25.6},
            {size: 5, price: 30.0},
        ].forEach(value =>
            it(`should return the price of ${value.price} for ${value.size} books`, () => {
                expect(priceBundle(value.size)).to.equal(value.price);
            }),
        );
    });
});
