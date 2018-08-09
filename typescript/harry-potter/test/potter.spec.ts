import {expect} from 'chai';
import {countBy, identity, pipe, sortBy, values} from 'lodash/fp';
import {describe, it} from 'mocha';

function priceBundle(bundleSize: number) {
    return [8, 15.2, 21.6, 25.6, 30][bundleSize - 1];
}

function groupBooks(books: number[]) {
    return pipe(
        countBy(identity),
        values,
        sortBy(identity))(books);
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

    describe('group distinct books in ascending order', () => {
        [
            {books: [], groups: []},
            {books: [1, 2, 3], groups: [1, 1, 1]},
            {books: [1, 1, 2, 2, 2, 3, 4], groups: [1, 1, 2, 3]},
        ].forEach(value =>
            it(`should group books ${value.books} to ${value.groups}`, () => {
                expect(groupBooks(value.books)).to.deep.equal(value.groups);
            }),
        );
    });
});
