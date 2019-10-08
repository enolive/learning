package de.welcz.reactivevalidation;

import lombok.Value;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Positive;
import java.math.BigDecimal;

@Value
public class Slave {
  @NotNull
  @Positive
  BigDecimal cost;
  @NotNull
  String name;
}
