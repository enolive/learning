import org.junit.Before;
import org.junit.Test;

import static org.assertj.core.api.Assertions.assertThat;

public class FizzBuzzTest {

    private FizzBuzzGenerator generator;

    @Before
    public void setUp() throws Exception {
        generator = new FizzBuzzGenerator();
    }

    @Test
    public void calculateShouldReturn1For1() {
        assertThat(generator.calculate(1)).isEqualTo("1");
    }
}
