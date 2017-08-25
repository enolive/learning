import com.mscharhag.oleaster.runner.OleasterRunner;
import org.junit.runner.RunWith;

import static com.mscharhag.oleaster.runner.StaticRunnerSupport.describe;
import static com.mscharhag.oleaster.runner.StaticRunnerSupport.it;
import static org.assertj.core.api.Assertions.assertThat;

@RunWith(OleasterRunner.class)
public class FizzBuzzTest {
    static {
        describe("Fizz Buzz Generator", () -> {
            it("should return normal numbers as is", () -> {
                FizzBuzzGenerator generator = new FizzBuzzGenerator();
                assertThat(generator.calculate(1)).isEqualTo("1");
                assertThat(generator.calculate(2)).isEqualTo("2");
            });

            it("should return Fizz for numbers divisible by 3", () -> {
                FizzBuzzGenerator generator = new FizzBuzzGenerator();
                assertThat(generator.calculate(3)).isEqualTo("Fizz");
                assertThat(generator.calculate(6)).isEqualTo("Fizz");
            });

            it("should return Buzz for numbers divisible by 5", () -> {
                FizzBuzzGenerator generator = new FizzBuzzGenerator();
                assertThat(generator.calculate(5)).isEqualTo("Buzz");
                assertThat(generator.calculate(10)).isEqualTo("Buzz");
            });

            it("should return Fizz-Buzz for numbers divisible by 3 and 5", () -> {
                FizzBuzzGenerator generator = new FizzBuzzGenerator();
                assertThat(generator.calculate(15)).isEqualTo("Buzz");
                assertThat(generator.calculate(30)).isEqualTo("Buzz");
            });
        });
    }
}
