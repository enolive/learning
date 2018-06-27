import Big from "big.js";
import {Book} from "./book";
import {Maybe} from "./maybe";

export class Bundle {
    private static sizeToDiscount = new Map<number, number>([
        [2, 0.95],
        [3, 0.90],
        [4, 0.80],
        [5, 0.75],
    ]);

    private books: Map<number, Book>;

    private constructor(private entries: ReadonlyArray<[number, Book]> = []) {
        this.books = new Map<number, Book>(entries);
    }

    get price(): Big {
        return new Big(8).mul(this.size).mul(this.discount);
    }

    get size(): number {
        return this.books.size;
    }

    private get discount(): number {
        return Maybe
            .of(Bundle.sizeToDiscount.get(this.size))
            .orElseGet(() => 1.0);
    }

    static empty() {
        return new Bundle();
    }

    add(book: Book): Bundle {
        if (this.has(book)) {
            return this;
        }
        const newEntries: ReadonlyArray<[number, Book]> = [[book.band, book], ...this.books.entries()];
        return new Bundle(newEntries);
    }

    has(book: Book) {
        return this.books.has(book.band);
    }
}
