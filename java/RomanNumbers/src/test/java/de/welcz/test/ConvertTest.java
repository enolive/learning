package de.welcz.test;

import com.mscharhag.oleaster.runner.OleasterRunner;
import de.welcz.Convert;
import org.junit.runner.RunWith;

import static com.mscharhag.oleaster.runner.StaticRunnerSupport.describe;
import static com.mscharhag.oleaster.runner.StaticRunnerSupport.it;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

@SuppressWarnings("ClassInitializerMayBeStatic") // HINT: non-static initializer required by OleasterRunner
@RunWith(OleasterRunner.class)
public class ConvertTest {
    {
        //noinspection CodeBlock2Expr - it would violate OCP
        describe("Converting an invalid digit", () -> {
            it("should fail", () -> {
                // arrange
                Class<IllegalArgumentException> expectedException = IllegalArgumentException.class;
                // act & assert
                assertThatExceptionOfType(expectedException)
                        .isThrownBy(() -> Convert.romanToArabic('A'))
                        .withMessage("Character 'A' represents no valid roman number");
                assertThatExceptionOfType(expectedException)
                        .isThrownBy(() -> Convert.romanToArabic('B'))
                        .withMessage("Character 'B' represents no valid roman number");
            });
        });

        describe("Converting a valid digit", () -> {
            it("should convert I to 1", () -> assertThat(Convert.romanToArabic('I')).isEqualTo(1));

            it("should convert V to 5", () -> assertThat(Convert.romanToArabic('V')).isEqualTo(5));

            it("should convert X to 10", () -> assertThat(Convert.romanToArabic('X')).isEqualTo(10));

            it("should convert L to 50", () -> assertThat(Convert.romanToArabic('L')).isEqualTo(50));

            it("should convert C to 100", () -> assertThat(Convert.romanToArabic('C')).isEqualTo(100));

            it("should convert D to 500", () -> assertThat(Convert.romanToArabic('D')).isEqualTo(500));

            it("should convert M to 1000", () -> assertThat(Convert.romanToArabic('M')).isEqualTo(1000));
        });
    }

}
