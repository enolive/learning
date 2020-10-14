package de.welcz.r2dbc.converter;

import de.welcz.r2dbc.api.ModifyProduct;
import de.welcz.r2dbc.api.ProductModel;
import de.welcz.r2dbc.persistence.ProductEntity;
import org.springframework.stereotype.Service;

import java.util.Objects;
import java.util.function.Function;

@Service
public class ProductConverter {
  public Function<ModifyProduct, ProductEntity> convertToEntityWithId(int id) {
    return modifyProduct -> {
      var entity = convertToEntity(modifyProduct);
      entity.setId(id);
      return entity;
    };
  }

  public ProductModel convertToDto(ProductEntity productEntity) {
    return new ProductModel(Objects.requireNonNull(productEntity.getId()),
                            productEntity.getDescription(),
                            productEntity.getPrice());
  }

  public ProductEntity convertToEntity(ModifyProduct modifyProduct) {
    var productEntity = new ProductEntity();
    productEntity.setDescription(modifyProduct.getDescription());
    productEntity.setPrice(modifyProduct.getPrice());
    return productEntity;
  }
}
