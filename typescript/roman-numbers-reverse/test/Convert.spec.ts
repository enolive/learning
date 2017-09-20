import {expect} from 'chai'
import {Arabic} from '../src/Arabic'

describe('convert arabic to roman', () => {
    describe('addition rules', () => {
        it('should return empty string for zero', () => {
            expect(Arabic.toRomanNumber(0)).to.equal('')
        })

        it('should return I for arabic less than 4', () => {
            expect(Arabic.toRomanNumber(1)).to.be.equal('I')
            expect(Arabic.toRomanNumber(2)).to.be.equal('II')
            expect(Arabic.toRomanNumber(3)).to.be.equal('III')
        })

        it('should return V for arabic less than 9', () => {
            expect(Arabic.toRomanNumber(5)).to.equal('V')
            expect(Arabic.toRomanNumber(6)).to.equal('VI')
            expect(Arabic.toRomanNumber(8)).to.equal('VIII')
        })

        it('should return X for arabic less than 40', () => {
            expect(Arabic.toRomanNumber(10)).to.equal('X')
            expect(Arabic.toRomanNumber(15)).to.equal('XV')
            expect(Arabic.toRomanNumber(18)).to.equal('XVIII')
        })

        it('should return L for arabic less than 90', () => {
            expect(Arabic.toRomanNumber(50)).to.equal('L')
            expect(Arabic.toRomanNumber(52)).to.equal('LII')
            expect(Arabic.toRomanNumber(75)).to.equal('LXXV')
        })

        it('should return C for arabic less than 400', () => {
            expect(Arabic.toRomanNumber(100)).to.equal('C')
            expect(Arabic.toRomanNumber(359)).to.equal('CCCLIX')
        })

        it('should return D for arabic less than 900', () => {
            expect(Arabic.toRomanNumber(500)).to.equal('D')
            expect(Arabic.toRomanNumber(871)).to.equal('DCCCLXXI')
        })

        it('should return M for arabic above 1000', () => {
            expect(Arabic.toRomanNumber(1000)).to.equal('M')
            expect(Arabic.toRomanNumber(1458)).to.equal('MCDLVIII')
        })
    })

    describe('subtraction rules', () => {
        it('should return IV for arabic 4', () => {
            expect(Arabic.toRomanNumber(4)).to.equal('IV')
        })

        it('should return IX for arabic 9', () => {
            expect(Arabic.toRomanNumber(9)).to.equal('IX')
        })

        it('should return XL for arabic 40', () => {
            expect(Arabic.toRomanNumber(40)).to.equal('XL')
        })

        it('should return XC for arabic 90', () => {
            expect(Arabic.toRomanNumber(90)).to.equal('XC')
        })

        it('should return CD for arabic 400', () => {
            expect(Arabic.toRomanNumber(400)).to.equal('CD')
        })

        it('should return CM for arabic 900', () => {
            expect(Arabic.toRomanNumber(900)).to.equal('CM')
        })
    })

    describe('high-level tests', () => {
        it('should convert 1984', () => {
            expect(Arabic.toRomanNumber(1984)).to.equal('MCMLXXXIV')
        })
    })
})
