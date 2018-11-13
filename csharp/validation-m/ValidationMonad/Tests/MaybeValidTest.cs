using System;
using FluentAssertions;
using Xunit;
using static Tests.Prelude;

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
            var success = Success<int, string>(42).Bimap(value => value + 1, error => error.ToLower());
            var failure = Failure<int, string>("Error").Bimap(value => value + 1, error => error.ToLower());
            success.Should().Be(Success<int, string>(43));
            failure.Should().Be(Failure<int, string>("error"));
        }

        [Fact]
        public void FilterShouldWork()
        {
            var willFail = Success<int, string>(42).FilterOrElse(_ => false, _ => "Error");
            var willSucceed = Success<int, string>(42).FilterOrElse(_ => true, _ => "Not an Error");
            willFail.IsFailure.Should().BeTrue();
            willFail.IsSuccess.Should().BeFalse();
            willSucceed.IsFailure.Should().BeFalse();
            willSucceed.IsSuccess.Should().BeTrue();
        }

        [Fact]
        public void FilterShouldWorkMultipleTimes()
        {
            Failure<int, string>("Error").FilterOrElse(_ => true, _ => "Should not be overwritten")
                .Should().Be(Failure<int, string>("Error"));
        }

        [Fact]
        public void FlatMapFailureShouldWork()
        {
            Success<int, string>(42).FlatMapFailure(_ => Failure<int, string>("not an error")).Should()
                .Be(Success<int, string>(42));
            Success<int, string>(42)
                .FlatMapFailure(_ => Failure<int, string>("not an error"), Identity).Should()
                .Be(Success<int, string>(42));
            Success<int, string>(42)
                .FlatMapFailure(_ => Failure<string, string>("not an error"), value => value.ToString())
                .Should()
                .Be(Success<string, string>("42"));
            Failure<int, string>("Error").FlatMapFailure(err => Success<int, string>(42)).Should()
                .Be(Success<int, string>(42));
            Failure<int, string>("Error")
                .FlatMapFailure(err => Success<int, string>(42), Identity).Should()
                .Be(Success<int, string>(42));
            Failure<int, string>("Error")
                .FlatMapFailure(err => Failure<string, Error>(new Error(err)), _ => "new value").Should()
                .Be(Failure<string, Error>(new Error("Error")));
        }

        [Fact]
        public void FlatMapShouldWork()
        {
            Success<int, string>(42).FlatMap(value => Success<int, string>(value + 1), Identity)
                .Should()
                .Be(Success<int, string>(43));
            Success<int, string>(42).FlatMap(value => Success<int, string>(value + 1)).Should()
                .Be(Success<int, string>(43));
            Success<int, string>(42).FlatMap(_ => Failure<int, string>("Error"), Identity)
                .Should()
                .Be(Failure<int, string>("Error"));
            Failure<int, string>("Error")
                .FlatMap(value => Success<int, string>(value + 1), Identity).Should()
                .Be(Failure<int, string>("Error"));
            Failure<int, string>("Error")
                .FlatMap(value => Success<int, Error>(value + 1), err => new Error(err)).Should()
                .Be(Failure<int, Error>(new Error("Error")));
        }

        [Fact]
        public void GetErrorShouldWork()
        {
            Success<int, string>(42).GetErrorOrElse("Not an Error").Should().Be("Not an Error");
            Success<int, string>(42).GetErrorOrElse(() => "Not an Error").Should().Be("Not an Error");
            Failure<int, string>("Error").GetErrorOrElse("Not an Error").Should().Be("Error");
            Failure<int, string>("Error").GetErrorOrElse(() => "Not an Error").Should().Be("Error");
        }

        [Fact]
        public void GetOrElseThrowShouldWork()
        {
            Action fail = () => Failure("Error").GetOrElseThrow(err => new InvalidOperationException(err));
            Success(42).GetOrElseThrow(_ => new InvalidOperationException("should not happen")).Should().Be(42);
            fail.Should().Throw<InvalidOperationException>().WithMessage("Error");
        }

        [Fact]
        public void GetValueShouldWork()
        {
            Success<int, string>(42).GetOrElse(0).Should().Be(42);
            Success<int, string>(42).GetOrElse(() => 0).Should().Be(42);
            Failure<int, string>("Error").GetOrElse(-1).Should().Be(-1);
            Failure<int, string>("Error").GetOrElse(() => -1).Should().Be(-1);
        }

        [Fact]
        public void MapFailureShouldWork()
        {
            Success<int, string>(42)
                .MapFailure(_ => "My new Failure").Should()
                .Be(Success<int, string>(42));
            Failure<int, string>("Error")
                .MapFailure(error => error.ToLower()).Should()
                .Be(Failure<int, string>("error"));
        }

        [Fact]
        public void MapShouldWork()
        {
            Success<int, string>(42).Map(value => value + 1).Should().Be(Success<int, string>(43));
            Failure<int, string>("Error").Map(value => value * 2).Should()
                .Be(Failure<int, string>("Error"));
            Success<int, string>(42).Map<int?>(_ => null).Should().Be(Success<int?, string>(null));
        }

        [Fact]
        public void SwapShouldWork()
        {
            Success<int, string>(42).Swap().Should().Be(Failure<string, int>(42));
            Failure<int, string>("Error").Swap().Should().Be(Success<string, int>("Error"));
        }
    }
}