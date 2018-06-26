import Big from "big.js";
import {filter, flow, map, minBy, reduce} from "lodash/fp";
import {Book} from "./book";
import {Bundle} from "./bundle";
import {Maybe} from "./maybe";

interface IAddToBundle {
    addToBundle(): boolean;
}

export class Basket {
    private bundles: Bundle[] = [];

    get price(): Big {
        return flow(
            map((bundle: Bundle) => bundle.price),
            reduce((a: Big, b: Big) => a.add(b))(new Big(0)),
        )(this.bundles);
    }

    add(book: Book) {
        this.bundleThatDoesNotContain(book).addToBundle();
    }

    private bundleThatDoesNotContain(book: Book): IAddToBundle {
        const getSmallestBundle = flow(
            filter((bundle: Bundle) => !bundle.has(book)),
            minBy((bundle: Bundle) => bundle.size),
        );
        return {
            addToBundle: () => Maybe
                .of(getSmallestBundle(this.bundles))
                .orElseGet(() => this.addNewBundle())
                .add(book),
        };
    }

    private addNewBundle() {
        const newBundle = new Bundle();
        this.bundles = [newBundle, ...this.bundles];
        return newBundle;
    }
}
