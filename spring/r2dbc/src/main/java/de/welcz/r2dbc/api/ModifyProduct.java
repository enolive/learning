package de.welcz.r2dbc.api;

import lombok.Value;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

@Value
public class ModifyProduct {
  @Size(min = 1)
  @NotNull
  String description;
  double price;
}
