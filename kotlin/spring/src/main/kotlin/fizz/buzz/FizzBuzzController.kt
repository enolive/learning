package fizz.buzz

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.HttpStatus
import org.springframework.web.bind.annotation.*

@RestController
class FizzBuzzController @Autowired
constructor(private val fizzBuzz: FizzBuzzService) {
    @ResponseStatus(HttpStatus.BAD_REQUEST, reason = "invalid parameters")
    @ExceptionHandler(IllegalArgumentException::class)
    fun handleIllegalArguments() {
    }
    
    @RequestMapping("/fizz-buzz/numbers/{limit}")
    fun getNumbers(@PathVariable("limit") limit: Int): Array<String> {
        require(limit in 1..1000) { "limit must be between 1 and 1000" }
        return fizzBuzz.calculateUpTo(limit)
    }
}
