import {Book} from "./book";

export class Bundle {
    private books: Book[] = [];

    get size(): number {
        return this.books.length;
    }

    add(book: Book) {
        if (this.books.some(b => b.band === book.band)) {
            return;
        }
        this.books.push(book);
    }
}
