import {countBy, identity, map, pipe, range, sortBy, sum, values, zip} from 'lodash/fp';

export function priceBundle(bundleSize: number) {
    return [8, 15.2, 21.6, 25.6, 30][bundleSize - 1];
}

export function groupBooks(books: number[]) {
    return pipe(
        countBy(identity),
        values,
        sortBy(identity))(books);
}

export function bundleGreedy(groups: number[]) {
    function bundleGreedyRec(used: number, result: number[], remainingGroups: number[]) {
        if (remainingGroups.length === 0) {
            return result;
        }
        const [head, ...tail] = remainingGroups;
        return bundleGreedyRec(head, [head - used, ...result], tail);
    }

    return bundleGreedyRec(0, [], groups);
}

export function adjust(bundles: number[]) {
    const [bundle1, bundle2, bundle3, bundle4, bundle5] = bundles;
    const commonSize = Math.min(bundle3, bundle5) || 0;
    if (commonSize !== 0) {
        return [bundle1, bundle2, bundle3 - commonSize, bundle4 + commonSize * 2, bundle5 - commonSize];
    }
    return bundles;
}

export function priceBundles(tuple: number[]) {
    const [bundleSize, bundleCount] = tuple;
    return priceBundle(bundleSize) * bundleCount;
}

function getBundlePrices(bundles: []) {
    const zipBundlesWithSize = zip(range(1, bundles.length + 1))(bundles);
    return map(priceBundles)(zipBundlesWithSize);
}

export function getPriceFor(books: number[]) {
    return pipe(
        groupBooks,
        bundleGreedy,
        adjust,
        getBundlePrices,
        sum,
    )(books);
}
