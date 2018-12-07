package de.welcz.fizzbuzz;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/fizz-buzz/")
public class FizzBuzzController {
    private final FizzBuzzService service;

    @Autowired
    public FizzBuzzController(FizzBuzzService service) {
        this.service = service;
    }

    @GetMapping("/numbers/{input}")
    public String getNumber(@PathVariable int input) {
        return service.calculate(input);
    }
}
