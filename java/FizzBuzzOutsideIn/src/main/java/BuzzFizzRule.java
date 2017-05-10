public class BuzzFizzRule implements Rule {
    @Override
    public boolean appliesTo(int number) {
        return number % 15 == 0;
    }

    @Override
    public String getResult() {
        return "Buzz-Fizz";
    }
}
