public class BuzzRule implements Rule {


    @Override
    public boolean appliesTo(int number) {
       return number % 5 == 0;
    }

    @Override
    public String getResult() {
        return "Buzz";
    }
}
