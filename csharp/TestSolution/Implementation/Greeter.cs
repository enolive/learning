namespace Implementation
{
    public class Greeter
    {
        public string SayHello() => SayHelloTo("World");

        public string SayHelloTo(string name) => $"Hello {name}!";
    }
}