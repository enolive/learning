class Bowling(object):
    def __init__(self):
        self.rolls = []

    def roll(self, pins: int) -> None:
        self.rolls.append(pins)

    def get_score(self) -> int:
        score = 0
        ball_index = 0
        for frame in range(0, 10):
            if self.rolls[ball_index] + self.rolls[ball_index + 1] == 10:
                score += 10 + self.rolls[ball_index + 2]
                ball_index += 2
            else:
                score += self.rolls[ball_index] + self.rolls[ball_index + 1]
                ball_index += 2
        return score
