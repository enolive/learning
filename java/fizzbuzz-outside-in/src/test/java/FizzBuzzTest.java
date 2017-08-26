import com.mscharhag.oleaster.runner.OleasterRunner;
import org.junit.runner.RunWith;

import static com.mscharhag.oleaster.runner.StaticRunnerSupport.describe;
import static com.mscharhag.oleaster.runner.StaticRunnerSupport.it;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Mockito.*;

@RunWith(OleasterRunner.class)
public class FizzBuzzTest {
    static {
        describe("Fizz Buzz Generator", () -> {
            describe("Outside-in", () -> {
                it("should check whether each rule applies to the input", () -> {
                    int input = aNumber();
                    Rule rule1 = aRule();
                    Rule rule2 = aRule();
                    FizzBuzzGenerator generator = new FizzBuzzGenerator(rule1, rule2);
                    generator.calculate(input);
                    verify(rule1).appliesTo(input);
                    verify(rule2).appliesTo(input);
                });
                
                it("should stop checking if one matching rule was found", () -> {
                    Rule rule1 = aMatchingRule();
                    Rule rule2 = aRule();
                    FizzBuzzGenerator generator = new FizzBuzzGenerator(rule1, rule2);
                    generator.calculate(aNumber());
                    verify(rule2, never()).appliesTo(anyInt());
                });
                
                it("should return the result of the matching rule", () -> {
                    Rule rule = aMatchingRule();
                    String text = aText();
                    when(rule.getResult()).thenReturn(text);
                    FizzBuzzGenerator generator = new FizzBuzzGenerator(rule);
                    assertThat(generator.calculate(aNumber())).isEqualTo(text);
                });
                
                it("should return the input itself when no rule is matching", () -> {
                   FizzBuzzGenerator generator = new FizzBuzzGenerator(aRule());
                   assertThat(generator.calculate(23)).isEqualTo("23");
                });
            });
            
            it("should return expected values", () -> {
                FizzBuzzGenerator generator = new FizzBuzzGenerator();
                assertThat(generator.calculate(1)).isEqualTo("1");
                assertThat(generator.calculate(2)).isEqualTo("2");
                assertThat(generator.calculate(3)).isEqualTo("Fizz");
                assertThat(generator.calculate(5)).isEqualTo("Buzz");
                assertThat(generator.calculate(15)).isEqualTo("Fizz-Buzz");
            });
            
            it("zero should be returned as zero", () -> {
               FizzBuzzGenerator generator = new FizzBuzzGenerator();
               assertThat(generator.calculate(0)).isEqualTo("0");
            });
        });
    }

    private static String aText() {
        return "Something";
    }

    private static Rule aMatchingRule() {
        Rule rule = aRule();
        when(rule.appliesTo(anyInt())).thenReturn(true);
        return rule;
    }

    private static int aNumber() {
        return 42;
    }

    private static Rule aRule() {
        Rule mock = mock(Rule.class);
        when(mock.getResult()).thenReturn("");
        return mock;
    }
    
}
