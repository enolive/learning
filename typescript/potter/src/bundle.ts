import {Book} from "./book";

export class Bundle {
    private books = new Map<number, Book>();

    get size(): number {
        return this.books.size;
    }

    add(book: Book) {
        this.books.set(book.band, book);
    }
}
