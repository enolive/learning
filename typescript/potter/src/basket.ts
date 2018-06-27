import Big from "big.js";
import {filter, flow, map, minBy, reduce, without} from "lodash/fp";
import {Book} from "./book";
import {Bundle} from "./bundle";
import {Maybe} from "./maybe";

export class Basket {
    private constructor(private bundles: Bundle[] = []) {
    }

    get price(): Big {
        const getSumOfPrices = flow(
            map((bundle: Bundle) => bundle.price),
            reduce((a: Big, b: Big) => a.add(b))(new Big(0)),
        );
        return getSumOfPrices(this.bundles);
    }

    static empty() {
        return new Basket();
    }

    add(book: Book): Basket {
        const getSmallestBundle = flow(
            filter((bundle: Bundle) => !bundle.has(book)),
            minBy((bundle: Bundle) => bundle.size),
        );
        const smallestBundle = Maybe
            .of(getSmallestBundle(this.bundles))
            .orElseGet(() => Bundle.empty());
        const other = without([smallestBundle])(this.bundles);
        return new Basket([smallestBundle.add(book), ...other]);
    }
}
