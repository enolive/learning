import Big from "big.js";
import {Book} from "./book";
import {Bundle} from "./bundle";

export class Basket {
    private bundles: Bundle[] = [];

    get price(): Big {
        return this.bundles
            .map(bundle => bundle.price)
            .reduce((a, b) => a.add(b), new Big(0));
    }

    private static smallestSize(a: Bundle, b: Bundle) {
        return a.size - b.size;
    }

    add(book: Book) {
        this.bundleThatDoesNotContain(book).add(book);
        this.sortBySizeAscending();
    }

    private sortBySizeAscending() {
        this.bundles.sort((a, b) => Basket.smallestSize(a, b));
    }

    private bundleThatDoesNotContain(book: Book): Bundle {
        const emptyBundle = this.bundles.find(bundle => !bundle.has(book));
        if (emptyBundle) {
            return emptyBundle;
        }
        const newBundle = new Bundle();
        this.bundles.push(newBundle);
        return newBundle;
    }
}
