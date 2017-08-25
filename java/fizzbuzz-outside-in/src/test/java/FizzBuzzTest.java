import com.mscharhag.oleaster.runner.OleasterRunner;
import org.junit.runner.RunWith;

import static com.mscharhag.oleaster.runner.StaticRunnerSupport.describe;
import static com.mscharhag.oleaster.runner.StaticRunnerSupport.it;
import static org.assertj.core.api.Assertions.assertThat;

@RunWith(OleasterRunner.class)
public class FizzBuzzTest {
    static {
        describe("Fizz Buzz Generator", () -> {
            describe("Outside-in", () -> {
                it("should do something", () -> {});
            });
            it("should return 1 for 1", () -> {
                FizzBuzzGenerator generator = new FizzBuzzGenerator();
                assertThat(generator.calculate(1)).isEqualTo("1");
            });
        });
    }
}
