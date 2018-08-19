using System;

namespace retry_task
{
    internal static class TimespanExtensions
    {
        public static TimeSpan MilliSeconds(this int milliSeconds)
        {
            return TimeSpan.FromMilliseconds(milliSeconds);
        }
    }
}