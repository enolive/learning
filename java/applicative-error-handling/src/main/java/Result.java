import lombok.Getter;
import lombok.ToString;

import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

@ToString
@Getter
class Result<Error, T> {
    private T item;
    private Error error;

    private Result(Error error, T item) {
        this.error = error;
        this.item = item;
    }

    public static <Error, T> Result<Error, T> error(Error error) {
        Objects.requireNonNull(error, "error must not be null.");
        return new Result<>(error, null);
    }

    public static <Error, T> Result<List<Error>, List<T>> sequence(Iterable<Result<Error, T>> results) {
        Objects.requireNonNull(results, "results must not be null.");
        List<T> allSuccesses = asStream(results).filter(Result::isSuccess)
                                                .map(Result::getItem)
                                                .collect(Collectors.toList());
        List<Error> allErrors = asStream(results).filter(Result::isError)
                                                 .map(Result::getError)
                                                 .collect(Collectors.toList());
        return new Result<>(allErrors, allSuccesses);
    }

    private static <Error, T> Stream<Result<Error, T>> asStream(Iterable<Result<Error, T>> results) {
        return StreamSupport.stream(results.spliterator(), false);
    }

    private boolean isSuccess() {
        return item != null;
    }

    private boolean isError() {
        return error != null;
    }

    public static <Error, T> Result<Error, List<T>> flattenSuccess(Result<Error, List<List<T>>> result) {
        Objects.requireNonNull(result, "result must not be null.");
        List<T> flattened = Optional.ofNullable(result.getItem())
                                    .map(Result::flattenList)
                                    .orElse(null);
        Error error = result.error;
        return new Result<>(error, flattened);
    }

    private static <T> List<T> flattenList(List<List<T>> lists) {
        return lists.stream()
                    .flatMap(Collection::stream)
                    .collect(Collectors.toList());
    }

    public static <Error, T> Result<Error, T> success(T item) {
        Objects.requireNonNull(item, "item must not be null.");
        return new Result<>(null, item);
    }
}
