import Big from "big.js";
import {Book} from "./book";

export class Bundle {
    private books = new Map<number, Book>();
    get discount(): number {
        switch (this.size) {
            case 2:
                return 0.95;
            default:
                return 1.0;
        }
    }

    get price(): Big {
        return new Big(8).mul(this.size).mul(this.discount);
    }

    get size(): number {
        return this.books.size;
    }

    add(book: Book) {
        this.books.set(book.band, book);
    }
}
