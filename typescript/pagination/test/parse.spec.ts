import {expect} from 'chai';
import {parseLinkHeader} from '../index';

describe('Pagination Links', () => {
    it('should parse missing link tag', () => {
        expect(parseLinkHeader(null)).to.deep.equal([]);
        expect(parseLinkHeader(undefined)).to.deep.equal([]);
    });
    it('should parse empty link tag', () => {
        expect(parseLinkHeader('')).to.deep.equal([]);
    });
    it('should parse single link', () => {
        expect(parseLinkHeader('</myurl>;rel=self')).to.deep.equal([{rel: 'self', url: '/myurl'}]);
    });
    it('should parse single link with >\'s', () => {
        expect(parseLinkHeader('</myurl>>>;rel=self')).to.deep.equal([{rel: 'self', url: '/myurl>>'}]);
    });
    it('should reject links with valid url infix', () => {
        expect(parseLinkHeader('invalid<myurl>invalid;rel=self')).to.deep.equal([]);
    });
    it('should reject links with valid rel infix', () => {
        expect(parseLinkHeader('<myurl>;invalidrel=self')).to.deep.equal([]);
    });
    it('should reject invalid urls', () => {
        expect(parseLinkHeader('invalid;rel=self')).to.deep.equal([]);
    });
    it('should reject invalid rels', () => {
        expect(parseLinkHeader('</myurl>;invalid')).to.deep.equal([]);
    });
    it('should parse multiple links', () => {
        expect(parseLinkHeader('</myurl>;rel=self,</myurl2>;rel=next')).to.deep.equal(
            [
                {rel: 'self', url: '/myurl'},
                {rel: 'next', url: '/myurl2'},
            ]);
    });
    it('should parse multiple links with quotes', () => {
        expect(parseLinkHeader('</myurl>;rel="self",</myurl2>;rel="next"')).to.deep.equal(
            [
                {rel: 'self', url: '/myurl'},
                {rel: 'next', url: '/myurl2'},
            ]);
    });
    it('should parse multiple links with whitespaces', () => {
        expect(parseLinkHeader(' </myurl> ; rel = self , </myurl2> ; rel = next ')).to.deep.equal(
            [
                {rel: 'self', url: '/myurl'},
                {rel: 'next', url: '/myurl2'},
            ]);
    });
});