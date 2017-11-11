class Bowling(object):
    def __init__(self):
        self.rolls = []

    def roll(self, pins: int) -> None:
        self.rolls.append(pins)

    def get_score(self) -> int:
        score, ball_index = 0, 0
        for frame in range(0, 10):
            score, ball_index = self.score_frame(ball_index, score)
        return score

    def score_frame(self, ball_index, score):
        if self.is_strike(ball_index):
            ball_index, score = self.score_strike(ball_index, score)
        elif self.is_spare(ball_index):
            ball_index, score = self.score_spare(ball_index, score)
        else:
            ball_index, score = self.score_normal_frame(ball_index, score)
        return score, ball_index

    def is_strike(self, ball_index):
        return self.rolls[ball_index] == 10

    def is_spare(self, ball_index):
        return self.get_frame_score(ball_index) == 10

    def score_normal_frame(self, ball_index, score):
        score += self.rolls[ball_index] + self.rolls[ball_index + 1]
        ball_index += 2
        return ball_index, score

    def score_spare(self, ball_index, score):
        score += 10 + self.rolls[ball_index + 2]
        ball_index += 2
        return ball_index, score

    def score_strike(self, ball_index, score):
        score += 10 + self.get_frame_score(ball_index + 1)
        ball_index += 1
        return ball_index, score

    def get_frame_score(self, ball_index: int) -> int:
        return self.rolls[ball_index] + self.rolls[ball_index + 1]
