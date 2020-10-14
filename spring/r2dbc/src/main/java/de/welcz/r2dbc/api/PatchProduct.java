package de.welcz.r2dbc.api;

import lombok.Value;

import javax.validation.constraints.Size;

@Value
public class PatchProduct {
  @Size(min = 1)
  String description;
  Double price;
}
