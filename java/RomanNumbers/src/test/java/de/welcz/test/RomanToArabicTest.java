package de.welcz.test;

import com.mscharhag.oleaster.runner.OleasterRunner;
import de.welcz.RomanToArabicConverter;
import org.junit.runner.RunWith;

import static com.mscharhag.oleaster.runner.StaticRunnerSupport.*;
import static org.assertj.core.api.Assertions.assertThat;

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
        });
    }
}
