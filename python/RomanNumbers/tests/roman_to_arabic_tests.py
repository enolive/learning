import unittest

from assertpy import assert_that
from nose_parameterized import parameterized

from implementation.converter import RomanToArabic
from implementation.exceptions import IllegalArgumentError
from tests.method_conversion import as_function


class RomanToArabicTests(unittest.TestCase):
    def setUp(self):
        self.target = RomanToArabic()

    @parameterized.expand([
        ("I", 1),
        ("II", 2),
        ("III", 3),
    ])
    def test_that_I_should_be_added_to_result(self, number, expected):
        # act
        result = self.target.to_arabic(number)
        # assert
        assert_that(result).is_equal_to(expected)

    def test_that_invalid_characters_fail(self):
        assert_that(as_function(self.target.to_arabic)) \
            .raises(IllegalArgumentError) \
            .when_called_with("ThisIsNotRoman") \
            .is_equal_to("'ThisIsNotRoman' contains characters that are not a roman digit.")

    @parameterized.expand([
        ("V", 5),
        ("VI", 6),
    ])
    def test_that_V_should_be_added_to_result(self, number, expected):
        # act
        result = self.target.to_arabic(number)
        # assert
        assert_that(result).is_equal_to(expected)

    @parameterized.expand([
        ("X", 10),
        ("XII", 12),
        ("XV", 15),
    ])
    def test_that_X_should_be_added_to_result(self, number, expected):
        # act
        result = self.target.to_arabic(number)
        # assert
        assert_that(result).is_equal_to(expected)\

    @parameterized.expand([
        ("IX", 9),
    ])
    def test_that_X_should_be_added_to_result(self, number, expected):
        # act
        result = self.target.to_arabic(number)
        # assert
        assert_that(result).is_equal_to(expected)
