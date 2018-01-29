package hello

import org.springframework.http.HttpStatus
import org.springframework.web.bind.annotation.ControllerAdvice
import org.springframework.web.bind.annotation.ExceptionHandler
import org.springframework.web.bind.annotation.ResponseStatus

@ControllerAdvice
class GlobalExceptionHandler {
    @ResponseStatus(
            value = HttpStatus.BAD_REQUEST,
            reason = "illegal arguments detected")
    @ExceptionHandler(IllegalArgumentException::class)
    fun handleBadRequests() {
    }
}