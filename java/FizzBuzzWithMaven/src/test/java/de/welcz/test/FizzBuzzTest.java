package de.welcz.test;

import com.mscharhag.oleaster.runner.OleasterRunner;
import de.welcz.FizzBuzzEngine;
import org.junit.runner.RunWith;

import static com.mscharhag.oleaster.runner.StaticRunnerSupport.*;
import static org.assertj.core.api.Assertions.assertThat;

@RunWith(OleasterRunner.class)
public class FizzBuzzTest {
    private FizzBuzzEngine engine;

    {
        describe("Fizz Buzz Engine", () -> {

            beforeEach(() -> engine = new FizzBuzzEngine());

            it("should return normal numbers as is", () -> {
                assertThat(engine.calculateNext(1)).isEqualTo("1");
                assertThat(engine.calculateNext(2)).isEqualTo("2");
                assertThat(engine.calculateNext(4)).isEqualTo("4");
            });

            it("should return numbers divisible by 3 as Fizz", () -> {
                assertThat(engine.calculateNext(3)).isEqualTo("Fizz");
                assertThat(engine.calculateNext(6)).isEqualTo("Fizz");
            });

            it("should return numbers divisible by 5 as Buzz", () -> {
                assertThat(engine.calculateNext(5)).isEqualTo("Buzz");
                assertThat(engine.calculateNext(10)).isEqualTo("Buzz");
            });

            it("should return numbers divisible by both 3 and 5 as Fizz-Buzz", () -> {
                assertThat(engine.calculateNext(15)).isEqualTo("Fizz-Buzz");
                assertThat(engine.calculateNext(30)).isEqualTo("Fizz-Buzz");
            });
        });
    }
}
