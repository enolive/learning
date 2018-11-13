using System;
using System.Collections.Generic;

namespace Tests
{
    public class Success<T, TError> : IMaybeValid<T, TError>
    {
        public T GetOrElseThrow<TException>(Func<TError, TException> getException) where TException : Exception =>
            Value;

        public Success(T value)
        {
            Value = value;
        }

        public T Value { get; }
        public IMaybeValid<TError, T> Swap() => new Failure<TError, T>(Value);

        public IMaybeValid<TNew, TNewError> FlatMap<TNew, TNewError>(
            Func<T, IMaybeValid<TNew, TNewError>> bindFunc,
            Func<TError, TNewError> projectFunc) =>
            bindFunc(Value);

        public IMaybeValid<TNew, TError> FlatMap<TNew>(
            Func<T, IMaybeValid<TNew, TError>> bindFunc) =>
            FlatMap(bindFunc, Prelude.Identity);

        public IMaybeValid<T, TNewError> FlatMapFailure<TNewError>(Func<TError, IMaybeValid<T, TNewError>> bindFunc) =>
            FlatMapFailure(bindFunc, Prelude.Identity);

        public IMaybeValid<TNew, TNewError> FlatMapFailure<TNew, TNewError>(
            Func<TError, IMaybeValid<TNew, TNewError>> bindFunc, Func<T, TNew> projectFunc) =>
            new Success<TNew, TNewError>(projectFunc(Value));

        public TError GetErrorOrElse(TError other) => other;

        public IMaybeValid<TValue, TError> Map<TValue>(Func<T, TValue> mapFunc) =>
            new Success<TValue, TError>(mapFunc(Value));

        public IMaybeValid<T, TNewError> MapFailure<TNewError>(Func<TError, TNewError> mapFunc) =>
            new Success<T, TNewError>(Value);

        public IMaybeValid<TNew, TNewError> Bimap<TNew, TNewError>(Func<T, TNew> successMapFunc,
            Func<TError, TNewError> failureMapFunc) =>
            new Success<TNew, TNewError>(successMapFunc(Value));

        public T GetOrElse(Func<T> otherFunc) => Value;

        public TError GetErrorOrElse(Func<TError> otherFunc) => otherFunc();

        public IMaybeValid<T, TError> FilterOrElse(Func<T, bool> predicate, Func<T, TError> zero) =>
            predicate(Value)
                ? (IMaybeValid<T, TError>) this
                : new Failure<T, TError>(zero(Value));

        public T GetOrElse(T other) => Value;

        public bool IsFailure { get; } = false;
        public bool IsSuccess { get; } = true;

        private bool Equals(Success<T, TError> other) => EqualityComparer<T>.Default.Equals(Value, other.Value) &&
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

            return Equals((Success<T, TError>) obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = EqualityComparer<T>.Default.GetHashCode(Value);
                hashCode = (hashCode * 397) ^ IsFailure.GetHashCode();
                hashCode = (hashCode * 397) ^ IsSuccess.GetHashCode();
                return hashCode;
            }
        }
    }
}