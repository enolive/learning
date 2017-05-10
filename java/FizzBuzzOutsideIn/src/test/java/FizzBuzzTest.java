import com.mscharhag.oleaster.runner.OleasterRunner;
import org.junit.runner.RunWith;

import static com.mscharhag.oleaster.runner.StaticRunnerSupport.describe;
import static com.mscharhag.oleaster.runner.StaticRunnerSupport.it;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Mockito.*;

@RunWith(OleasterRunner.class)
public class FizzBuzzTest {
    {
        describe("FizzBuzz", () -> {

            //Test goes here
            it("should return 1 when input is 1", () -> {
                FizzBuzz fizzBuzz = new FizzBuzz();
                String result = fizzBuzz.convert(1);
                assertThat(result).isEqualTo("1");
            });

            it("should check that all rules are invoked", () -> {
                Rule rule1 = mock(Rule.class);
                Rule rule2 = mock(Rule.class);
                FizzBuzz fizzBuzz = new FizzBuzz(rule1, rule2);
                fizzBuzz.convert(27);
                verify(rule1).appliesTo(27);
                verify(rule2).appliesTo(27);
            });

            it("should return the rule result if rule applies", () -> {
                Rule rule1 = mock(Rule.class);
                FizzBuzz fizzBuzz = new FizzBuzz(rule1);
                when(rule1.appliesTo(42)).thenReturn(true);
                when(rule1.getResult()).thenReturn("blafasel");
                String result = fizzBuzz.convert(42);
                assertThat(result).isEqualTo("blafasel");
            });
            it("should return the number if no rule applies", () -> {
                Rule rule1 = mock(Rule.class);
                FizzBuzz fizzBuzz = new FizzBuzz(rule1);
                when(rule1.appliesTo(anyInt())).thenReturn(false);

                String result = fizzBuzz.convert(4711);

                assertThat(result).isEqualTo("4711");
                verify(rule1, never()).getResult();
            });

            it("should return fizz if number is 3", () -> {

                FizzBuzz fizzBuzz = new FizzBuzz();

                String result = fizzBuzz.convert(3);

                assertThat(result).isEqualTo("Fizz");

            });


            it("should return buzz if number is 5", () -> {

                FizzBuzz fizzBuzz = new FizzBuzz();

                String result = fizzBuzz.convert(5);

                assertThat(result).isEqualTo("Buzz");

            });


            it("should return buzz-fizz if number is 15", () -> {

                FizzBuzz fizzBuzz = new FizzBuzz();

                String result = fizzBuzz.convert(15);

                assertThat(result).isEqualTo("Buzz-Fizz");

            });

        });
    }

}
