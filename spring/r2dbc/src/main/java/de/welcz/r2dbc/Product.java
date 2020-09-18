package de.welcz.r2dbc;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;
import lombok.ToString;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.Transient;
import org.springframework.data.domain.Persistable;

@Data
@ToString
public class Product implements Persistable<Integer> {
  @Id
  private Integer id;
  private String description;
  private Double price;

  @Override
  @Transient
  @JsonIgnore
  public boolean isNew() {
    return id == null;
  }

  public Product setAsNew() {
    this.id = null;
    return this;
  }
}
