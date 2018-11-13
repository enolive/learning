namespace Tests
{
    public static class Prelude
    {
        public static IMaybeValid<T, TError> Failure<T, TError>(TError error)
        {
            return new Failure<T, TError>(error);
        }

        public static IMaybeValid<Empty, TError> Failure<TError>(TError error)
        {
            return new Failure<Empty, TError>(error);
        }

        public static IMaybeValid<T, TError> Success<T, TError>(T value)
        {
            return new Success<T, TError>(value);
        }

        public static IMaybeValid<T, Empty> Success<T>(T value)
        {
            return new Success<T, Empty>(value);
        }

        public static T Identity<T>(T arg) => arg;

        public abstract class Empty
        {
        }
    }
}