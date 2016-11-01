package de.welcz.test;

import com.mscharhag.oleaster.runner.OleasterRunner;
import de.welcz.Digit;
import org.junit.runner.RunWith;

import static com.mscharhag.oleaster.runner.StaticRunnerSupport.describe;
import static com.mscharhag.oleaster.runner.StaticRunnerSupport.it;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

@RunWith(OleasterRunner.class)
public class DigitTest {
    {

        describe("Digit index", () -> {
            it("should retrieve the right position", () -> {
                // act
                int result = digitFrom("XXVIII", 2).getArabicRepresentation();
                // assert
                assertThat(result).isEqualTo(5);
            });

            it("should fail on not existing position", () -> {
                // arrange
                Class<IndexOutOfBoundsException> expectedException = IndexOutOfBoundsException.class;
                // act & assert
                assertThatExceptionOfType(expectedException).isThrownBy(() -> digitFrom("X", 3));
                assertThatExceptionOfType(expectedException).isThrownBy(() -> digitFrom("X", -1));
            });
        });

        describe("Digit followed by higher digit", () -> {
            it("should have higher follower", () -> {
                // act & assert
                assertThat(firstDigitFrom("IV").hasHigherFollower()).isTrue();
                assertThat(firstDigitFrom("IIX").hasHigherFollower()).isTrue();
            });

            it("should have a negative arabic representation", () -> {
                // act & assert
                assertThat(firstDigitFrom("IV").getArabicRepresentation())
                        .as("these digits are expected to be subtracted")
                        .isEqualTo(-1);
            });
        });

        describe("Digit followed by lower digit", () -> {
            it("should have no higher follower", () -> {
                // act & assert
                assertThat(firstDigitFrom("VI").hasHigherFollower()).isFalse();
                assertThat(firstDigitFrom("XXV").hasHigherFollower()).isFalse();
                assertThat(digitFrom("XXVI", 2).hasHigherFollower()).as("V is followed by I").isFalse();
                assertThat(digitFrom("XXVI", 3).hasHigherFollower()).as("it is the last digit").isFalse();
            });

            it("should have a positive arabic representation", () -> {
                // act & assert
                assertThat(firstDigitFrom("XI").getArabicRepresentation()).isEqualTo(10);
                assertThat(digitFrom("XVI", 1).getArabicRepresentation()).isEqualTo(5);
            });
        });
    }

    private Digit firstDigitFrom(String roman) {
        return digitFrom(roman, 0);
    }

    private Digit digitFrom(String roman, int index) {
        return new Digit(roman, index);
    }
}