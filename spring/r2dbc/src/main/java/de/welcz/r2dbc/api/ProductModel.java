package de.welcz.r2dbc.api;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.EqualsAndHashCode;
import lombok.Value;
import org.springframework.hateoas.RepresentationModel;

@EqualsAndHashCode(callSuper = true)
@Value
public class ProductModel extends RepresentationModel<ProductModel> {
  @JsonIgnore
  int id;
  String description;
  double price;
}
