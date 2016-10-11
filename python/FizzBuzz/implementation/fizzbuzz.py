class Engine(object):
    def __init__(self):
        self.rules = [
            (15, "Fizz-Buzz"),
            (3, "Fizz"),
            (5, "Buzz"),
        ]

    def calculate_result(self, number):
        find_matching_rule = (result
                              for (nominator, result)
                              in self.rules
                              if Engine.is_divisible_by(number, nominator))
        return next(find_matching_rule, str(number))

    @staticmethod
    def is_divisible_by(number, nominator):
        return number % nominator == 0
