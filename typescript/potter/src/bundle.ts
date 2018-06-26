import Big from "big.js";
import {Book} from "./book";
import {Maybe} from "./maybe";

export class Bundle {
    private books = new Map<number, Book>();
    private sizeToDiscount = new Map<number, number>([
        [2, 0.95],
        [3, 0.90],
        [4, 0.85],
        [5, 0.80],
    ]);

    get price(): Big {
        return new Big(8).mul(this.size).mul(this.discount);
    }

    get size(): number {
        return this.books.size;
    }

    private get discount(): number {
        return Maybe
            .of(this.sizeToDiscount.get(this.size))
            .orElseGet(() => 1.0);
    }

    add(book: Book): boolean {
        if (this.has(book)) {
            return false;
        }
        this.books.set(book.band, book);
        return true;
    }

    has(book: Book) {
        return this.books.has(book.band);
    }
}
