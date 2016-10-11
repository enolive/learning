from implementation.exceptions import IllegalArgumentError


class RomanToArabic(object):
    def __init__(self):
        self.terms_for_roman = {
            'I': 1,
            'V': 5,
            'X': 10,
        }

    def to_arabic(self, roman):
        try:
            arabic_values = map(self.get_addition_term, roman)
            return sum(arabic_values)
        except KeyError:
            raise IllegalArgumentError("'{0}' contains characters that are not a roman digit.".format(roman))

    def get_addition_term(self, digit):
        return self.terms_for_roman[digit]
