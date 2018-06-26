export class Maybe<T> {
    static just(value) {
        return new Maybe(value);
    }

    static nothing() {
        return new Maybe(null);
    }

    static of(value) {
        return value ? Maybe.just(value) : Maybe.nothing();
    }

    private constructor(private readonly value: T) {
    }

    orElseGet(alternative: () => T): T {
        return this.value === null ? alternative() : this.value;
    }
}
