import Big from "big.js";
import {Book} from "./book";
import {Bundle} from "./bundle";

export class Basket {
    private bundles: Bundle = new Bundle();

    get price(): Big {
        return this.bundles.price;
    }

    add(book: Book) {
        this.bundles.add(book);
    }
}
