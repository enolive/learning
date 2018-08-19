using System;
using System.Threading.Tasks;

namespace retry_task
{
    internal static class Retry
    {
        public static async Task<T> ExecuteAsync<T>(Func<T> func, RetryConfiguration configuration)
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
}