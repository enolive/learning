import {Book} from "./book";

export class Bundle {
    private _size: number = 0;

    get size(): number {
        return this._size;
    }

    add(book: Book) {
        this._size++;
    }
}
