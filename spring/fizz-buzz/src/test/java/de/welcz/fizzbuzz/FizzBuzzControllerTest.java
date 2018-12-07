package de.welcz.fizzbuzz;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest
@ExtendWith(SpringExtension.class)
class FizzBuzzControllerTest {
    @Autowired
    private MockMvc mockMvc;

    @Test
    void getNumber() throws Exception {
        mockMvc.perform(get("/api/v1/fizz-buzz/numbers/42"))
               .andExpect(status().isOk())
               .andExpect(content().string("number!"));
    }
}
