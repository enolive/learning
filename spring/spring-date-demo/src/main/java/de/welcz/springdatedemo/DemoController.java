package de.welcz.springdatedemo;

import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import static java.text.MessageFormat.format;

@RestController
@RequestMapping("/api/v1")
public class DemoController {
    @GetMapping("/date/{date}")
    public String simpleDate(@PathVariable
                             @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate date) {
        return format("Hello, {0}!", date);
    }

    @GetMapping("/basicdate/{date}")
    public String simpleBasicDate(@PathVariable
                             @DateTimeFormat(pattern = "yyyyMMdd") LocalDate date) {
        return format("Hello, {0}!", date);
    }

    @GetMapping("/datetime/{dateTime}")
    public String simpleDate(@PathVariable
                             @DateTimeFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss.SSS") LocalDateTime dateTime) {
        return format("Hello, {0}!", dateTime);
    }

    @GetMapping("/json")
    public String json(@RequestBody RequestData data) {
        DateTimeFormatter shortTime = DateTimeFormatter.ofPattern("HH:mm");
        return format("Hello, {0} {1} {2}!",
                      data.getName(),
                      data.getDateTime().toLocalDate(),
                      data.getDateTime().toLocalTime().format(shortTime));
    }
}
