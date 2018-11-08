import {fromNullable, none, Option, some} from 'fp-ts/lib/Option';

export interface ILinkDescription {
    url: string;
    rel: string;
}

export const parseLinkHeader = (header: string): ILinkDescription[] =>
    fromNullable(header)
        .map(p => p.trim()
                   .split(',')
                   .map(toLinkDescriptionOrNull)
                   .filter(defined))
        .getOrElse([]);

const toLinkDescriptionOrNull = (link: string): ILinkDescription =>
    some(link)
        .map(l => l.trim().split(';'))
        .chain(tryMakeTuple)
        .chain(tryExtractParts)
        .getOrElse(null);

const tryMakeTuple = ([url, rel]: string[]): Option<ILinkDescription> =>
    some({url, rel})
        .filter(bothPartsDefined)
        .map(some)
        .getOrElse(none);

const tryExtractParts = (tuple: ILinkDescription): Option<ILinkDescription> =>
    some(tuple)
        .map(({url, rel}) => ({url: extractUrlPart(url), rel: extractRelPart(rel)}))
        .filter(bothPartsDefined)
        .map(some)
        .getOrElse(none);

const bothPartsDefined = ({url, rel}: ILinkDescription): boolean =>
    defined(url) && defined(rel);

const defined = (thing): boolean => !!thing;

const extractUrlPart = (url: string): string => {
    return some(url).map(u => u.match(/^\s*<(.*)>\s*$/))
                    .filter(defined)
                    .map(u => u[1])
                    .getOrElse(null);
};

const extractRelPart = (rel: string): string =>
    some(rel).map(r => r.match(/^\s*rel\s*=\s*"?([^"]*)"?\s*/))
             .filter(defined)
             .map(r => r[1])
             .getOrElse(null);
