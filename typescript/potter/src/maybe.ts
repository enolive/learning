export class Maybe<T> {
    private constructor(private readonly value: T) {
    }

    static just(value) {
        return new Maybe(value);
    }

    static nothing() {
        return new Maybe(null);
    }

    static of(value) {
        return value === null || value === undefined
            ? Maybe.nothing()
            : Maybe.just(value);
    }

    orElseGet(alternative: () => T): T {
        return this.value === null ? alternative() : this.value;
    }
}
