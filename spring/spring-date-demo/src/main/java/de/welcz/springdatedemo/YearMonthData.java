package de.welcz.springdatedemo;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Value;

import java.time.YearMonth;

@Value
public class YearMonthData {
    @JsonProperty("name")
    private final String name;
    @JsonProperty("date")
    // jackson can even properly parse Year-Month values!
    @JsonFormat(pattern = "yyyyMM")
    private final YearMonth yearAndMonth;
}
