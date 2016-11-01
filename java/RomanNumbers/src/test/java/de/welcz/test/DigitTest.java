package de.welcz.test;

import com.mscharhag.oleaster.runner.OleasterRunner;
import de.welcz.Digit;
import org.junit.runner.RunWith;

import static com.mscharhag.oleaster.runner.StaticRunnerSupport.describe;
import static com.mscharhag.oleaster.runner.StaticRunnerSupport.it;
import static org.assertj.core.api.Assertions.assertThat;

@SuppressWarnings("ClassInitializerMayBeStatic") // HINT: non-static initializer required by OleasterRunner
@RunWith(OleasterRunner.class)
public class DigitTest {
    {
        describe("A Roman Digit", () -> {
            it("should convert I to 1", () -> {
                // act
                int result = aDigit('I').getArabicRepresentation();
                // assert
                assertThat(result).isEqualTo(1);
            });

            it("should convert V to 5", () -> {
                // act
                int result = aDigit('V').getArabicRepresentation();
                // assert
                assertThat(result).isEqualTo(5);
            });

            it("should convert X to 10", () -> {
                // act
                int result = aDigit('X').getArabicRepresentation();
                // assert
                assertThat(result).isEqualTo(10);
            });
        });
    }

    private Digit aDigit(char romanDigit) {
        return new Digit("" + romanDigit, 0);
    }
}
