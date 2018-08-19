using System;
using System.Threading.Tasks;

namespace retry_task
{
    class Program
    {
        private static readonly Random Random = new Random();

        static void Main(string[] args)
        {
            Retry(DoSomething, 5).Wait();
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
            return Random.Next(0, 2) == 0;
        }

        private static async Task<T> Retry<T>(Func<T> func, int retryCount)
        {
            while (true)
            {
                try
                {
                    var result = await Task.Run(func);
                    return result;
                }
                catch when (retryCount-- > 0)
                {
                }
            }
        }
    }

    internal class Unit
    {
        public static Unit Value { get; } = new Unit();
    }
}