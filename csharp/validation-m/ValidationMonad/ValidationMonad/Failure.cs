using System;
using System.Collections.Generic;
using static ValidationMonad.Prelude;

namespace ValidationMonad
{
    public class Failure<T, TError> : IMaybeValid<T, TError>
    {
        public Failure(TError error)
        {
            Error = error;
        }

        public T GetOrElseThrow<TException>(Func<TError, TException> getException)
            where TException : Exception => throw getException(Error);

        public TError Error { get; }
        public IMaybeValid<TError, T> Swap() => new Success<TError, T>(Error);

        public bool IsFailure { get; } = true;

        public bool IsSuccess { get; } = false;

        public IMaybeValid<T, TError> FilterOrElse(Func<T, bool> predicate, Func<T, TError> zero) => this;

        public IMaybeValid<TNew, TNewError> Bimap<TNew, TNewError>(Func<T, TNew> successMapFunc,
            Func<TError, TNewError> failureMapFunc) =>
            new Failure<TNew, TNewError>(failureMapFunc(Error));

        public T GetOrElse(Func<T> otherFunc) => otherFunc();

        public TError GetErrorOrElse(Func<TError> otherFunc) => Error;

        public TError GetErrorOrElse(TError other) => Error;

        public IMaybeValid<TValue, TError> Map<TValue>(Func<T, TValue> mapFunc) =>
            new Failure<TValue, TError>(Error);

        public IMaybeValid<TNew, TNewError> FlatMap<TNew, TNewError>(
            Func<T, IMaybeValid<TNew, TNewError>> bindFunc,
            Func<TError, TNewError> projectFunc) => new Failure<TNew, TNewError>(projectFunc(Error));

        public IMaybeValid<T, TNewError> FlatMapFailure<TNewError>(Func<TError, IMaybeValid<T, TNewError>> bindFunc) =>
            FlatMapFailure(bindFunc, Identity);

        public IMaybeValid<TNew, TNewError> FlatMapFailure<TNew, TNewError>(
            Func<TError, IMaybeValid<TNew, TNewError>> bindFunc, Func<T, TNew> projectFunc) => bindFunc(Error);

        public IMaybeValid<TNew, TError> FlatMap<TNew>(Func<T, IMaybeValid<TNew, TError>> bindFunc) =>
            FlatMap(bindFunc, Identity);

        public IMaybeValid<T, TNewError> MapFailure<TNewError>(Func<TError, TNewError> mapFunc) =>
            new Failure<T, TNewError>(mapFunc(Error));

        public T GetOrElse(T other) => other;

        private bool Equals(Failure<T, TError> other) => EqualityComparer<TError>.Default.Equals(Error, other.Error) &&
                                                         IsFailure == other.IsFailure && IsSuccess == other.IsSuccess;

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj))
            {
                return false;
            }

            if (ReferenceEquals(this, obj))
            {
                return true;
            }

            if (obj.GetType() != GetType())
            {
                return false;
            }

            return Equals((Failure<T, TError>) obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = EqualityComparer<TError>.Default.GetHashCode(Error);
                hashCode = (hashCode * 397) ^ IsFailure.GetHashCode();
                hashCode = (hashCode * 397) ^ IsSuccess.GetHashCode();
                return hashCode;
            }
        }
    }
}