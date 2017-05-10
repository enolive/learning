import com.mscharhag.oleaster.runner.OleasterRunner;
import org.junit.runner.RunWith;

import static com.mscharhag.oleaster.runner.StaticRunnerSupport.describe;
import static com.mscharhag.oleaster.runner.StaticRunnerSupport.it;
import static org.assertj.core.api.Assertions.assertThat;

@RunWith(OleasterRunner.class)
public class GreeterTest {{
    describe("Hello World", () -> {
        it("should say hello to world", () -> {
            Greeter greeter = new Greeter();
            String message = greeter.sayHello();
            assertThat(message).isEqualTo("Hello, World!");
        });
    });
}}
