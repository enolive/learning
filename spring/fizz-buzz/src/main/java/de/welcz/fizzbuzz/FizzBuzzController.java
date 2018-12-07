package de.welcz.fizzbuzz;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/fizz-buzz/")
public class FizzBuzzController {
    @GetMapping("/numbers/{input}")
    public String getNumber(@PathVariable int input) {
        return "number!";
    }
}
