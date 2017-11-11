class Bowling(object):
    def __init__(self):
        self.rolls = []

    def roll(self, pins: int) -> None:
        self.rolls.append(pins)

    @property
    def get_score(self) -> int:
        score, ball_index = 0, 0
        for frame in range(0, 10):
            score, ball_index = self.score_frame(ball_index, score)
        return score

    def score_frame(self, ball_index: int, score: int) -> (int, int):
        ball_index, score = self.get_scoring_function(ball_index)(ball_index, score)
        return score, ball_index

    def get_scoring_function(self, ball_index):
        if self.is_strike(ball_index):
            return self.score_strike
        elif self.is_spare(ball_index):
            return self.score_spare
        else:
            return self.score_normal_frame

    def is_strike(self, ball_index: int) -> bool:
        return self.rolls[ball_index] == 10

    def is_spare(self, ball_index: int) -> bool:
        return self.get_frame_score(ball_index) == 10

    def score_strike(self, ball_index: int, score: int) -> (int, int):
        return ball_index + 1, score + 10 + self.get_frame_score(ball_index + 1)

    def score_spare(self, ball_index: int, score: int) -> (int, int):
        return ball_index + 2, score + 10 + self.rolls[ball_index + 2]

    def score_normal_frame(self, ball_index: int, score: int) -> (int, int):
        return ball_index + 2, score + self.get_frame_score(ball_index)

    def get_frame_score(self, ball_index: int) -> int:
        return self.rolls[ball_index] + self.rolls[ball_index + 1]
