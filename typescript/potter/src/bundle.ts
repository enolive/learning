import Big from "big.js";
import {Book} from "./book";

export class Bundle {
    private books = new Map<number, Book>();

    get discount(): number {
        switch (this.size) {
            case 2:
                return 0.95;
            case 3:
                return 0.9;
            case 4:
                return 0.85;
            case 5:
                return 0.80;
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
