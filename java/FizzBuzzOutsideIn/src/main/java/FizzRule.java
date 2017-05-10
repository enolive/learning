public class FizzRule implements Rule {
    @Override
    public boolean appliesTo(int number) {
        return number % 3 == 0;
    }

    @Override
    public String getResult() {
        return "Fizz";
    }
}
