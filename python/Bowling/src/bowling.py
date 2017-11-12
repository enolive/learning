from functools import reduce
from typing import Callable, Tuple


class Bowling(object):
    def __init__(self):
        self.__rolls = []

    def roll(self, pins: int) -> None:
        self.__rolls.append(pins)

    @property
    def score(self) -> int:
        _, score = reduce(
            lambda acc, _: self.__score_frame(acc),
            self.__all_frames(),
            (0, 0)
        )
        return score

    @staticmethod
    def __all_frames():
        return range(0, 10)

    def __score_frame(self, current_frame: Tuple[int, int]) -> (int, int):
        ball_index, score = current_frame
        return self.__get_scoring_function(ball_index)(score)

    def __get_scoring_function(self, ball_index: int) -> Callable[[int], Tuple[int, int]]:
        rules = (
            (self.__is_strike, self.__score_strike),
            (self.__is_spare, self.__score_spare),
            (lambda _: True, self.__score_normal_frame),
        )
        found_rule = next(map(lambda r: r[1], filter(lambda r: r[0](ball_index), rules)))
        return found_rule(ball_index)

    def __is_strike(self, ball_index: int) -> bool:
        return self.__get_ball_points(ball_index) == 10

    def __is_spare(self, ball_index: int) -> bool:
        return self.__get_frame_score(ball_index) == 10

    def __score_strike(self, ball_index: int) -> Callable[[int], Tuple[int, int]]:
        return lambda score: (ball_index + 1, score + 10 +
                              self.__get_ball_points(ball_index + 1) +
                              self.__get_ball_points(ball_index + 2))

    def __score_spare(self, ball_index: int) -> Callable[[int], Tuple[int, int]]:
        return lambda score: (ball_index + 2, score + 10 + self.__get_ball_points(ball_index + 2))

    def __score_normal_frame(self, ball_index: int) -> Callable[[int], Tuple[int, int]]:
        return lambda score: (ball_index + 2, score + self.__get_frame_score(ball_index))

    def __get_frame_score(self, ball_index: int) -> int:
        return self.__get_ball_points(ball_index) + self.__get_ball_points(ball_index + 1)

    def __get_ball_points(self, ball_index):
        return self.__rolls[ball_index] if len(self.__rolls) > ball_index else 0
