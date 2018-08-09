import {expect} from 'chai';
import {adjust, bundleGreedy, getPriceFor, groupBooks, priceBundle} from '../src/potter';

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

    describe('make highest possible bundles', () => {
        [
            {group: [1, 1, 2, 3, 4], bundle: [1, 1, 1, 0, 1]},
            {group: [1], bundle: [1]},
            {group: [1, 1], bundle: [0, 1]},
            {group: [1, 1, 3], bundle: [2, 0, 1]},
            {group: [], bundle: []},
        ].forEach(value =>
            it(`should bundle book groups of ${value.group} to ${value.bundle}`, () => {
                expect(bundleGreedy(value.group)).to.deep.equal(value.bundle);
            }),
        );
    });

    describe('adjust 3 and 5 bundles to 2x4', () => {
        [
            {original: [], adjusted: []},
            {original: [0, 0, 1, 0, 1], adjusted: [0, 0, 0, 2, 0]},
            {original: [1, 1, 1, 1, 1], adjusted: [1, 1, 0, 3, 0]},
            {original: [1], adjusted: [1]},
            {original: [1, 2, 3], adjusted: [1, 2, 3]},
        ].forEach(value =>
            it(`should adjust ${value.original} to ${value.adjusted}`, () => {
                expect(adjust(value.original)).to.deep.equal(value.adjusted);
            }),
        );
    });

    describe('integration test', () => {
        it('should get a price of 51.20', () => {
            expect(getPriceFor([1, 1, 2, 2, 3, 3, 4, 5])).to.equal(51.20);
        });
    });
});
