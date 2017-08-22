import org.junit.Test;

import static org.assertj.core.api.Assertions.assertThat;

public class FizzBuzzTest {

    @Test
    public void calculateShouldReturn1For1() {
        FizzBuzzGenerator generator = new FizzBuzzGenerator();
        assertThat(generator.calculate(1)).isEqualTo("1");
    }
}
