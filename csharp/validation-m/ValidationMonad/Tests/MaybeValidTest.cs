using System;
using FluentAssertions;
using Xunit;

namespace Tests
{
    public class MaybeValidTest
    {
        private struct Error
        {
            // ReSharper disable once UnusedAutoPropertyAccessor.Local
            public string Message { get; }

            public Error(string message)
            {
                Message = message;
            }
        }

        [Fact]
        public void BimapShouldWork()
        {
            var success = Prelude.Success<int, string>(42).Bimap(value => value + 1, error => error.ToLower());
            var failure = Prelude.Failure<int, string>("Error").Bimap(value => value + 1, error => error.ToLower());
            success.Should().Be(Prelude.Success<int, string>(43));
            failure.Should().Be(Prelude.Failure<int, string>("error"));
        }

        [Fact]
        public void FilterShouldWork()
        {
            var willFail = Prelude.Success<int, string>(42).FilterOrElse(_ => false, _ => "Error");
            var willSucceed = Prelude.Success<int, string>(42).FilterOrElse(_ => true, _ => "Not an Error");
            willFail.IsFailure.Should().BeTrue();
            willFail.IsSuccess.Should().BeFalse();
            willSucceed.IsFailure.Should().BeFalse();
            willSucceed.IsSuccess.Should().BeTrue();
        }

        [Fact]
        public void FilterShouldWorkMultipleTimes()
        {
            Prelude.Failure<int, string>("Error").FilterOrElse(_ => true, _ => "Should not be overwritten")
                .Should().Be(Prelude.Failure<int, string>("Error"));
        }

        [Fact]
        public void FlatMapFailureShouldWork()
        {
            Prelude.Success<int, string>(42).FlatMapFailure(_ => Prelude.Failure<int, string>("not an error")).Should()
                .Be(Prelude.Success<int, string>(42));
            Prelude.Success<int, string>(42)
                .FlatMapFailure(_ => Prelude.Failure<int, string>("not an error"), Prelude.Identity).Should()
                .Be(Prelude.Success<int, string>(42));
            Prelude.Success<int, string>(42)
                .FlatMapFailure(_ => Prelude.Failure<string, string>("not an error"), value => value.ToString())
                .Should()
                .Be(Prelude.Success<string, string>("42"));
            Prelude.Failure<int, string>("Error").FlatMapFailure(err => Prelude.Success<int, string>(42)).Should()
                .Be(Prelude.Success<int, string>(42));
            Prelude.Failure<int, string>("Error")
                .FlatMapFailure(err => Prelude.Success<int, string>(42), Prelude.Identity).Should()
                .Be(Prelude.Success<int, string>(42));
            Prelude.Failure<int, string>("Error")
                .FlatMapFailure(err => Prelude.Failure<string, Error>(new Error(err)), _ => "new value").Should()
                .Be(Prelude.Failure<string, Error>(new Error("Error")));
        }

        [Fact]
        public void FlatMapShouldWork()
        {
            Prelude.Success<int, string>(42).FlatMap(value => Prelude.Success<int, string>(value + 1), Prelude.Identity)
                .Should()
                .Be(Prelude.Success<int, string>(43));
            Prelude.Success<int, string>(42).FlatMap(value => Prelude.Success<int, string>(value + 1)).Should()
                .Be(Prelude.Success<int, string>(43));
            Prelude.Success<int, string>(42).FlatMap(_ => Prelude.Failure<int, string>("Error"), Prelude.Identity)
                .Should()
                .Be(Prelude.Failure<int, string>("Error"));
            Prelude.Failure<int, string>("Error")
                .FlatMap(value => Prelude.Success<int, string>(value + 1), Prelude.Identity).Should()
                .Be(Prelude.Failure<int, string>("Error"));
            Prelude.Failure<int, string>("Error")
                .FlatMap(value => Prelude.Success<int, Error>(value + 1), err => new Error(err)).Should()
                .Be(Prelude.Failure<int, Error>(new Error("Error")));
        }

        [Fact]
        public void GetErrorShouldWork()
        {
            Prelude.Success<int, string>(42).GetErrorOrElse("Not an Error").Should().Be("Not an Error");
            Prelude.Success<int, string>(42).GetErrorOrElse(() => "Not an Error").Should().Be("Not an Error");
            Prelude.Failure<int, string>("Error").GetErrorOrElse("Not an Error").Should().Be("Error");
            Prelude.Failure<int, string>("Error").GetErrorOrElse(() => "Not an Error").Should().Be("Error");
        }

        [Fact]
        public void GetOrElseThrowShouldWork()
        {
            Action fail = () => Prelude.Failure("Error").GetOrElseThrow(err => new InvalidOperationException(err));
            Prelude.Success(42).GetOrElseThrow(_ => new InvalidOperationException("should not happen")).Should().Be(42);
            fail.Should().Throw<InvalidOperationException>().WithMessage("Error");
        }

        [Fact]
        public void GetValueShouldWork()
        {
            Prelude.Success<int, string>(42).GetOrElse(0).Should().Be(42);
            Prelude.Success<int, string>(42).GetOrElse(() => 0).Should().Be(42);
            Prelude.Failure<int, string>("Error").GetOrElse(-1).Should().Be(-1);
            Prelude.Failure<int, string>("Error").GetOrElse(() => -1).Should().Be(-1);
        }

        [Fact]
        public void MapFailureShouldWork()
        {
            Prelude.Success<int, string>(42)
                .MapFailure(_ => "My new Failure").Should()
                .Be(Prelude.Success<int, string>(42));
            Prelude.Failure<int, string>("Error")
                .MapFailure(error => error.ToLower()).Should()
                .Be(Prelude.Failure<int, string>("error"));
        }

        [Fact]
        public void MapShouldWork()
        {
            Prelude.Success<int, string>(42).Map(value => value + 1).Should().Be(Prelude.Success<int, string>(43));
            Prelude.Failure<int, string>("Error").Map(value => value * 2).Should()
                .Be(Prelude.Failure<int, string>("Error"));
            Prelude.Success<int, string>(42).Map<int?>(_ => null).Should().Be(Prelude.Success<int?, string>(null));
        }

        [Fact]
        public void SwapShouldWork()
        {
            Prelude.Success<int, string>(42).Swap().Should().Be(Prelude.Failure<string, int>(42));
            Prelude.Failure<int, string>("Error").Swap().Should().Be(Prelude.Success<string, int>("Error"));
        }
    }
}