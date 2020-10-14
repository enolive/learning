package de.welcz.r2dbc.persistence;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.Transient;
import org.springframework.data.domain.Persistable;
import org.springframework.data.relational.core.mapping.Table;

import javax.validation.constraints.NotNull;

@Data
@Builder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
@Table("product")
public class ProductEntity implements Persistable<Integer> {
  @Id
  private Integer id;
  private String description;
  @NotNull
  private Double price;

  @Override
  @Transient
  @JsonIgnore
  public boolean isNew() {
    return id == null;
  }

  public ProductEntity newCopy() {
    var newProduct = new ProductEntity();
    newProduct.description = description;
    newProduct.price = price;
    return newProduct;
  }
}
