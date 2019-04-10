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
class Result<Error, Success> {
    private Error error;
    private Success success;

    private Result(Error error, Success success) {
        this.error = error;
        this.success = success;
    }

    public static <Error, Success> Result<Error, Success> error(Error error) {
        Objects.requireNonNull(error, "error must not be null.");
        return new Result<>(error, null);
    }

    public static <Error, Success> Result<Error, Success> success(Success item) {
        Objects.requireNonNull(item, "success must not be null.");
        return new Result<>(null, item);
    }

    public static <Error, Success> Result<List<Error>, List<Success>> sequence(Iterable<Result<Error, Success>> results) {
        Objects.requireNonNull(results, "results must not be null.");
        List<Success> allSuccesses = asStream(results).filter(Result::isSuccess)
                                                      .map(Result::getSuccess)
                                                      .collect(Collectors.toList());
        List<Error> allErrors = asStream(results).filter(Result::isError)
                                                 .map(Result::getError)
                                                 .collect(Collectors.toList());
        return new Result<>(allErrors, allSuccesses);
    }

    public static <Error, Success> Result<Error, List<Success>> flattenSuccess(Result<Error, List<List<Success>>> result) {
        Objects.requireNonNull(result, "result must not be null.");
        List<Success> flattened = Optional.ofNullable(result.getSuccess())
                                          .map(Result::flattenList)
                                          .orElse(null);
        Error error = result.error;
        return new Result<>(error, flattened);
    }

    private static <Error, Success> Stream<Result<Error, Success>> asStream(Iterable<Result<Error, Success>> results) {
        return StreamSupport.stream(results.spliterator(), false);
    }

    private static <T> List<T> flattenList(List<List<T>> lists) {
        return lists.stream()
                    .flatMap(Collection::stream)
                    .collect(Collectors.toList());
    }

    private boolean isSuccess() {
        return success != null;
    }

    private boolean isError() {
        return error != null;
    }
}
