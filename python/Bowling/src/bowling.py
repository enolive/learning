class Bowling(object):
    def __init__(self):
        self.rolls = []

    def roll(self, pins: int) -> None:
        self.rolls.append(pins)

    def get_score(self) -> int:
        score = 0
        ball_index = 0
        for frame in range(0, 10):
            if self.rolls[ball_index] == 10:
                ball_index, score = self.score_strike(ball_index, score)
            elif self.score_frame(ball_index) == 10:
                score += self.score_spare(ball_index)
                ball_index += 2
            else:
                score += self.score_frame(ball_index)
                ball_index += 2
        return score

    def score_strike(self, ball_index, score):
        score += 10 + self.rolls[ball_index + 1] + self.rolls[ball_index + 2]
        ball_index += 1
        return ball_index, score

    def score_frame(self, ball_index: int) -> int:
        return self.rolls[ball_index] + self.rolls[ball_index + 1]

    def score_spare(self, ball_index: int) -> int:
        return 10 + self.rolls[ball_index + 2]
