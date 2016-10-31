package de.welcz.test;

import com.mscharhag.oleaster.runner.OleasterRunner;
import de.welcz.Greeter;
import org.junit.runner.RunWith;

import static com.mscharhag.oleaster.runner.StaticRunnerSupport.*;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.AssertionsForClassTypes.assertThatThrownBy;

@RunWith(OleasterRunner.class)
public class GreeterTest {
    private Greeter target;

    {
        describe("A Greeter", () -> {
            beforeEach(() -> target = new Greeter());

            it("should greet the world", () -> {
                // act
                String result = target.sayHello();
                // assert
                assertThat(result).isEqualTo("Hello, World!");
            });

            it("should greet an individual", () -> {
                // act
                String result = target.sayHelloTo("Christoph");
                // assert
                assertThat(result).isEqualTo("Hello, Christoph!");
            });

            it("should fail on an empty name", () -> {
                // act & assert
                assertThatThrownBy(() -> target.sayHelloTo(null))
                        .isInstanceOf(IllegalArgumentException.class)
                        .hasMessage("name must not be null or empty");
                assertThatThrownBy(() -> target.sayHelloTo(""))
                        .isInstanceOf(IllegalArgumentException.class)
                        .hasMessage("name must not be null or empty");
            });
        });
    }

}