package de.welcz.reactivevalidation;

import lombok.Value;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;

@Value
class Master {
  @NotNull
  @Pattern(regexp = "^\\d{7}-\\d{5}")
  String clientId;
  @NotNull
  @Valid
  Slave slave;
}
