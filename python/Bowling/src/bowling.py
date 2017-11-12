from functools import reduce
from typing import Callable, Tuple


class Bowling(object):
    def __init__(self):
        self.rolls = []

    def roll(self, pins: int) -> None:
        self.rolls.append(pins)

    @property
    def get_score(self) -> int:
        _, score = reduce(
            lambda acc, _: self.score_frame(acc),
            self.all_frames(),
            (0, 0)
        )
        return score

    @staticmethod
    def all_frames():
        return range(0, 10)

    def score_frame(self, current_frame: Tuple[int, int]) -> (int, int):
        ball_index, score = current_frame
        return self.get_scoring_function(ball_index)(score)

    def get_scoring_function(self, ball_index):
        rules = (
            (self.is_strike, self.score_strike),
            (self.is_spare, self.score_spare),
            (lambda _: True, self.score_normal_frame),
        )
        found_rule = next(map(lambda r: r[1], filter(lambda r: r[0](ball_index), rules)))
        return found_rule(ball_index)

    def is_strike(self, ball_index: int) -> bool:
        return self.get_ball_points(ball_index) == 10

    def is_spare(self, ball_index: int) -> bool:
        return self.get_frame_score(ball_index) == 10

    def score_strike(self, ball_index: int) -> Callable[[int], Tuple[int, int]]:
        return lambda score: (ball_index + 1, score + 10 +
                              self.get_ball_points(ball_index + 1) +
                              self.get_ball_points(ball_index + 2))

    def score_spare(self, ball_index: int) -> Callable[[int], Tuple[int, int]]:
        return lambda score: (ball_index + 2, score + 10 + self.get_ball_points(ball_index + 2))

    def score_normal_frame(self, ball_index: int) -> Callable[[int], Tuple[int, int]]:
        return lambda score: (ball_index + 2, score + self.get_frame_score(ball_index))

    def get_frame_score(self, ball_index: int) -> int:
        return self.get_ball_points(ball_index) + self.get_ball_points(ball_index + 1)

    def get_ball_points(self, ball_index):
        return self.rolls[ball_index] if len(self.rolls) > ball_index else 0
