using System;

namespace retry_task
{
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
}