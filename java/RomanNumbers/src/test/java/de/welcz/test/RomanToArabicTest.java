package de.welcz.test;

import com.mscharhag.oleaster.runner.OleasterRunner;
import de.welcz.RomanToArabicConverter;
import org.junit.runner.RunWith;

import static com.mscharhag.oleaster.runner.StaticRunnerSupport.*;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

@RunWith(OleasterRunner.class)
public class RomanToArabicTest {

    private RomanToArabicConverter target;

    {
        describe("Converter for Roman Numbers", () -> {
            beforeEach(() -> target = new RomanToArabicConverter());

            it("should add 1 for each I", () -> {
                assertThat(target.toArabic("II")).isEqualTo(2);
                assertThat(target.toArabic("III")).isEqualTo(3);
            });

            it("should add 5 for a V", () -> {
                assertThat(target.toArabic("VI")).isEqualTo(6);
                assertThat(target.toArabic("VII")).isEqualTo(7);
                assertThat(target.toArabic("VIII")).isEqualTo(8);
            });

            it("should add 10 for a X", () -> {
                assertThat(target.toArabic("XII")).isEqualTo(12);
                assertThat(target.toArabic("XVI")).isEqualTo(16);
            });

            it("should subtract lower digits", () -> {
                assertThat(target.toArabic("IV")).isEqualTo(4);
                assertThat(target.toArabic("IX")).isEqualTo(9);
                assertThat(target.toArabic("IIX")).isEqualTo(8);
            });

            it("should fail on invalid roman numbers", () ->
                    assertThatExceptionOfType(IllegalArgumentException.class)
                            .isThrownBy(() -> target.toArabic("CRAP")));

            it("should convert Orwell's 1984", () -> {
                assertThat(target.toArabic("MCMXXCIV")).isEqualTo(1984);
                assertThat(target.toArabic("MCMLXXXIV")).isEqualTo(1984);
                assertThat(target.toArabic("MDCCCCLXXXIV")).isEqualTo(1984);
            });

            it("should convert my birth year", () -> {
                assertThat(target.toArabic("MCMLXXVIII")).isEqualTo(1978);
                assertThat(target.toArabic("MCMLXXIIX")).isEqualTo(1978);
            });
        });
    }
}
