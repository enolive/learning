using System;

namespace retry_task
{
    internal class Program
    {
        private static readonly Random Random = new Random();

        private static void Main()
        {
            var configuration = new RetryConfiguration(5, 500.MilliSeconds());
            Retry.ExecuteAsync(TryDisplayHelloMessage, configuration).Wait();
        }

        private static Unit TryDisplayHelloMessage()
        {
            if (ShouldFail())
            {
                Console.WriteLine("Failing");
                throw new InvalidOperationException("I AM ERROR");
            }

            Console.WriteLine("Hello World!");
            return Unit.Value;
        }

        private static bool ShouldFail() => Random.Next(0, 3) != 0;

        private class Unit
        {
            public static Unit Value { get; } = new Unit();
        }
    }
}