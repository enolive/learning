using Moq;

namespace TestCommons
{
    public static class MockExtension
    {
        public static Mock<T> ToMock<T>(this T instance) where T : class
        {
            return Mock.Get(instance);
        }
    }
}