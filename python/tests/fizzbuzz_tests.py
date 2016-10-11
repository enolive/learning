import unittest

from assertpy import assert_that
from nose_parameterized import parameterized

from impl.fizzbuzz import Engine


class FizzBuzzEngineTests(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()

    @parameterized.expand([
        (1, "1"),
        (2, "2"),
    ])
    def test_normal_numbers(self, number, expected):
        # act
        result = self.engine.calculate_result(number)

        # assert
        assert_that(result).is_equal_to(expected)

    @parameterized.expand([
        (3, "Fizz"),
        (6, "Fizz"),
    ])
    def test_numbers_divisible_by_3_return_Fizz(self, number, expected):
        # act
        result = self.engine.calculate_result(number)

        # assert
        assert_that(result).is_equal_to(expected)

    @parameterized.expand([
        (5, "Buzz"),
        (10, "Buzz"),
    ])
    def test_numbers_divisible_by_5_return_Buzz(self, number, expected):
        # act
        result = self.engine.calculate_result(number)

        # assert
        assert_that(result).is_equal_to(expected)

    @parameterized.expand([
        (15, "Fizz-Buzz"),
        (30l, "Fizz-Buzz"),
    ])
    def test_numbers_divisible_by_3_and_5_return_Fizz_Buzz(self, number, expected):
        # act
        result = self.engine.calculate_result(number)

        # assert
        assert_that(result).is_equal_to(expected)
