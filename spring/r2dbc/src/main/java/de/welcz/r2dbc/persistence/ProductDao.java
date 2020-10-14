package de.welcz.r2dbc.persistence;

import de.welcz.r2dbc.api.ModifyProduct;
import de.welcz.r2dbc.api.ProductModel;
import de.welcz.r2dbc.converter.ProductConverter;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Service
public class ProductDao {
  private final ProductRepository repository;
  private final ProductConverter converter;

  public ProductDao(ProductRepository repository, ProductConverter converter) {
    this.repository = repository;
    this.converter = converter;
  }

  @Transactional
  public Mono<ProductModel> create(Mono<ModifyProduct> product) {
    return product.map(converter::convertToEntity)
                  .flatMap(repository::save)
                  .map(converter::convertToDto);
  }

  @Transactional
  public Mono<ProductModel> update(int id, Mono<ModifyProduct> product) {
    return product.map(converter.convertToEntityWithId(id))
                  .flatMap(repository::save)
                  .map(converter::convertToDto);
  }

  public Mono<Void> delete(int id) {
    return repository.deleteById(id);
  }

  public Flux<ProductModel> findAll() {
    return repository.findAllByOrderById()
                     .map(converter::convertToDto);
  }

  public Mono<ProductModel> find(int id) {
    return repository.findById(id)
                     .map(converter::convertToDto);
  }
}
