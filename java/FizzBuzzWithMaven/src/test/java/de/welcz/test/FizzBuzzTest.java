package de.welcz.test;

import de.welcz.FizzBuzzEngine;
import junitparams.JUnitParamsRunner;
import junitparams.Parameters;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import static org.assertj.core.api.Assertions.assertThat;

@RunWith(JUnitParamsRunner.class)
public class FizzBuzzTest {

    private FizzBuzzEngine target;

    @Before
    public void setUp() throws Exception {
        target = new FizzBuzzEngine();
    }

    @Test
    @Parameters({
            "1,1",
            "2,2",
    })
    public void normalNumbersShouldBeReturnedAsIs(int number, String expectedResult) {
        // act
        String result = target.calculateNext(number);
        // assert
        assertThat(result).isEqualTo(expectedResult);
    }

    @Test
    @Parameters({
            "3",
            "6",
    })
    public void numbersDivisibleByThreeShouldReturnFizz(int number) {
        // act
        String result = target.calculateNext(number);
        // assert
        assertThat(result).isEqualTo("Fizz");
    }

    @Test
    @Parameters({
            "5",
            "10",
    })
    public void numbersDivisibleByFiveShouldReturnBuzz(int number) {
        // act
        String result = target.calculateNext(number);
        // assert
        assertThat(result).isEqualTo("Buzz");
    }

    @Test
    @Parameters({
            "15",
            "30",
    })
    public void numbersDivisibleByThreeAndFiveShouldReturnFizzBuzz(int number) {
        // act
        String result = target.calculateNext(number);
        // assert
        assertThat(result).isEqualTo("Fizz-Buzz");
    }
}
