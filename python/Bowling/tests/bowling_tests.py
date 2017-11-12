import unittest

from assertpy import assert_that

from src.bowling import Bowling


class BowlingTests(unittest.TestCase):
    def setUp(self):
        self.bowling = Bowling()

    def test_gutter_game(self):
        self.roll_many(0, 20)
        assert_that(self.score).is_equal_to(0)

    def test_one_point_game(self):
        self.roll_many(1, 20)
        assert_that(self.score).is_equal_to(20)

    def test_spare(self):
        self.roll_pins(5, 5, 7)
        self.roll_many(0, 17)
        assert_that(self.score).is_equal_to(24)

    def test_strike(self):
        self.roll_pins(10)
        self.roll_pins(4, 5)
        self.roll_many(0, 16)
        assert_that(self.score).is_equal_to(28)

    def test_perfect_game(self):
        self.roll_many(10, 12)
        assert_that(self.score).is_equal_to(300)

    @property
    def score(self):
        return self.bowling.score

    def roll_pins(self, *pins: int) -> None:
        for p in pins:
            self.bowling.roll(p)

    def roll_many(self, pins: int, times: int) -> None:
        for t in range(0, times):
            self.bowling.roll(pins)
