import Extensions.Division

class FizzBuzzGenerator() {
    def nextNumber(input: Int): String = {
        if (input isDivisibleBy 15) {
            return "Fizz-Buzz"
        }
        
        if (input isDivisibleBy 3) {
            return "Fizz"
        }
        
        if (input isDivisibleBy 5) {
            return "Buzz"
        }
        
        input.toString
    }
}
