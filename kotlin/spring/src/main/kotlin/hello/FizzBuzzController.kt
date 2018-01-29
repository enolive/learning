package hello

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController

@RestController
class FizzBuzzController @Autowired
constructor(private val fizzBuzz: FizzBuzzService) {
    @RequestMapping("/fizz-buzz/numbers/{limit}")
    fun getNumbers(@PathVariable("limit") limit: Int): Array<String> {
        return fizzBuzz.calculateUpTo(limit)
    }
}
