using System;

namespace ValidationMonad
{
    public interface IMaybeValid<T, TError>
    {
        bool IsFailure { get; }
        bool IsSuccess { get; }

        IMaybeValid<T, TError> FilterOrElse(Func<T, bool> predicate, Func<T, TError> zero);

        T GetOrElse(T other);
        T GetOrElse(Func<T> otherFunc);
        TError GetErrorOrElse(TError other);
        TError GetErrorOrElse(Func<TError> otherFunc);
        IMaybeValid<TNew, TError> Map<TNew>(Func<T, TNew> mapFunc);
        IMaybeValid<T, TNewError> MapFailure<TNewError>(Func<TError, TNewError> mapFunc);

        IMaybeValid<TNew, TNewError> Bimap<TNew, TNewError>(Func<T, TNew> successMapFunc,
            Func<TError, TNewError> failureMapFunc);

        IMaybeValid<TNew, TError> FlatMap<TNew>(Func<T, IMaybeValid<TNew, TError>> bindFunc);

        IMaybeValid<TNew, TNewError> FlatMap<TNew, TNewError>(Func<T, IMaybeValid<TNew, TNewError>> bindFunc,
            Func<TError, TNewError> projectFunc);

        IMaybeValid<T, TNewError> FlatMapFailure<TNewError>(Func<TError, IMaybeValid<T, TNewError>> bindFunc);

        IMaybeValid<TNew, TNewError> FlatMapFailure<TNew, TNewError>(
            Func<TError, IMaybeValid<TNew, TNewError>> bindFunc,
            Func<T, TNew> projectFunc);

        IMaybeValid<TError, T> Swap();
        T GetOrElseThrow<TException>(Func<TError, TException> getException) where TException : Exception;
    }
}