class Bowling(object):
    def __init__(self):
        self.score = 0

    def roll(self, pins: int) -> None:
        self.score += pins

    def get_score(self) -> int:
        return self.score
