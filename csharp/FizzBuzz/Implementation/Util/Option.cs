using System;

namespace Implementation
{
    public class Option<T> where T : class
    {
        private T Value { get; set; }

        public Option(T value)
        {
            Value = value;
        }

        public T GetOrElse(Func<T> elseValue)
        {
            return Value ?? elseValue();
        }
    }
}