import org.junit.Test;

import static org.assertj.core.api.Assertions.assertThat;

public class FizzBuzzTest {
    @Test
    public void calculateShouldReturn1For1() {
        FizzBuzzGenerator generator = new FizzBuzzGenerator();
        String result = generator.calculate(1);
        assertThat(result).isEqualTo("1");
    }
}
