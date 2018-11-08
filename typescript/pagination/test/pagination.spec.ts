import {expect} from 'chai';
import {fromNullable, none, some} from 'fp-ts/lib/Option';

interface ILinkDescription {
    url: string;
    rel: string;
}

function mkUrlRelTuple([url, rel]) {
    if (!url || !rel) {
        return none;
    }
    return some({url, rel});
}

function extractParts({url, rel}) {
    const urlMatch = url.match(/<(.*)>/);
    const relMatch = rel.match(/rel=(.*)/);
    if (!urlMatch || !relMatch) {
        return none;
    }
    return some({url: urlMatch[1], rel: relMatch[1]});
}

function mkLinkDescription(links: string): ILinkDescription {
    const parts = some(links)
        .map(l => l.split(';'))
        .chain(mkUrlRelTuple)
        .chain(extractParts)
        .getOrElse(null);
    return parts;
}

function notNull(link): boolean {
    return link !== null;
}

function parsePagination(pagination: string): ILinkDescription[] {
    return fromNullable(pagination).map(p => p.split(',')
                                              .map(mkLinkDescription)
                                              .filter(notNull))
                                   .getOrElse([]);
}

describe('Pagination Links', () => {
    it('should parse missing link tag', () => {
        expect(parsePagination(null)).to.deep.equal([]);
        expect(parsePagination(undefined)).to.deep.equal([]);
    });
    it('should parse empty link tag', () => {
        expect(parsePagination('')).to.deep.equal([]);
    });
    it('should parse single link', () => {
        expect(parsePagination('</myurl>;rel=self')).to.deep.equal([{rel: 'self', url: '/myurl'}]);
    });
    it('should parse single link with >\'s', () => {
        expect(parsePagination('</myurl>>>;rel=self')).to.deep.equal([{rel: 'self', url: '/myurl>>'}]);
    });
    it('should reject invalid urls', () => {
        expect(parsePagination('invalid;rel=self')).to.deep.equal([]);
    });
    it('should reject invalid rels', () => {
        expect(parsePagination('</myurl>;invalid')).to.deep.equal([]);
    });
    it('should parse multiple links', () => {
        expect(parsePagination('</myurl>;rel=self,</myurl2>;rel=next')).to.deep.equal(
            [
                {rel: 'self', url: '/myurl'},
                {rel: 'next', url: '/myurl2'},
            ]);
    });
});