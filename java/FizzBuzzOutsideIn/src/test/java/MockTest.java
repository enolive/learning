import com.mscharhag.oleaster.runner.OleasterRunner;
import org.junit.runner.RunWith;

import static com.mscharhag.oleaster.runner.StaticRunnerSupport.*;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.*;

@RunWith(OleasterRunner.class)
public class MockTest {
    private Greeter greeter;

    {
        describe("Mocks of Greeter", () -> {
            beforeEach(() -> greeter = mock(Greeter.class));

            it("should return the specified value", () -> {
                when(greeter.sayHello()).thenReturn("TEST");
                String message = greeter.sayHello();
                assertThat(message).isEqualTo("TEST");
            });

            it("should verify its calls", () -> {
                greeter.sayHello();
                greeter.sayHello();
                verify(greeter, times(2)).sayHello();
            });

            it("should honor parameters", () -> {
                when(greeter.sayHelloTo(anyString())).thenReturn("Something");
                when(greeter.sayHelloTo("Christoph")).thenReturn("Something else");

                String concreteMessage = greeter.sayHelloTo("Christoph");
                String simpleMessage = greeter.sayHelloTo("Horst");

                assertThat(concreteMessage).isEqualTo("Something else");
                assertThat(simpleMessage).isEqualTo("Something");

                verify(greeter, never()).sayHelloTo("Arschloch");
            });
        });
    }
}
