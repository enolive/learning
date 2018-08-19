using System;
using System.Threading.Tasks;

namespace retry_task
{
    class Program
    {
        private static readonly Random Random = new Random();

        static void Main()
        {
            var configuration = new RetryConfiguration(5, 500.MilliSeconds());
            Retry(DoSomething, configuration).Wait();
        }

        private static Unit DoSomething()
        {
            if (ShouldFail())
            {
                Console.WriteLine("Failing");
                throw new InvalidOperationException("I AM ERROR");
            }

            Console.WriteLine("Hello World!");
            return Unit.Value;
        }

        private static bool ShouldFail()
        {
            return Random.Next(0, 3) != 0;
        }

        private static async Task<T> Retry<T>(Func<T> func, RetryConfiguration configuration)
        {
            var remainingRetries = configuration.Retries;
            while (true)
            {
                try
                {
                    var result = await Task.Run(func);
                    return result;
                }
                catch when (remainingRetries-- > 0)
                {
                    await Task.Delay(configuration.DelayBeforeRetry);
                }
            }
        }
    }

    internal class RetryConfiguration
    {
        public int Retries { get; }
        public TimeSpan DelayBeforeRetry { get; }

        public RetryConfiguration(int retries, TimeSpan delayBeforeRetry)
        {
            Retries = retries;
            DelayBeforeRetry = delayBeforeRetry;
        }
    }

    internal class Unit
    {
        public static Unit Value { get; } = new Unit();
    }

    internal static class TimespanExtensions
    {
        public static TimeSpan MilliSeconds(this int milliSeconds)
        {
            return TimeSpan.FromMilliseconds(milliSeconds);
        }
    }
}