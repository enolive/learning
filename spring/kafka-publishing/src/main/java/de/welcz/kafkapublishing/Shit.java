package de.welcz.kafkapublishing;

import lombok.Value;

import javax.validation.constraints.NotNull;
import java.time.YearMonth;

@Value
class Shit {
  @NotNull
  String clientId;
  @NotNull
  YearMonth date;
}
