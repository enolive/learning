package de.welcz.springdatedemo;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Value;

import java.time.LocalDateTime;

@Value
public class RequestData {
    @JsonProperty("name")
    private final String name;
    @JsonProperty("date")
    // date time with milliseconds and time zone in the format Sign Hours Minutes --
    // see SimpleDataFormat for a complete definition of all recognized patterns
    @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss.SSSZ")
    private final LocalDateTime dateTime;
}
