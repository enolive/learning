package de.welcz.r2dbc.persistence;

import de.welcz.r2dbc.api.ModifyProduct;
import de.welcz.r2dbc.api.PatchProduct;
import de.welcz.r2dbc.api.ProductModel;
import de.welcz.r2dbc.converter.ProductConverter;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
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
  public Mono<ProductModel> create(ModifyProduct product) {
    return Mono.just(product)
               .map(converter::convertToEntity)
               .flatMap(repository::save)
               .map(converter::convertToDto);
  }

  @Transactional
  public Mono<ProductModel> updateTotal(int id, ModifyProduct product) {
    return Mono.just(product)
               .map(converter.convertToEntityWithId(id))
               .flatMap(repository::save)
               .map(converter::convertToDto);
  }

  @Transactional
  public Mono<ProductModel> updatePartial(int id, PatchProduct product) {
    return repository.findById(id)
                     .zipWith(Mono.just(product), converter::patchExistingEntity)
                     .flatMap(repository::save)
                     .map(converter::convertToDto);
  }

  public Mono<Void> delete(int id) {
    return repository.deleteById(id);
  }

  public Flux<ProductModel> findAll() {
    return repository.findAllByOrderById(Pageable.unpaged())
                     .map(converter::convertToDto);
  }

  @Transactional
  public Mono<Page<ProductModel>> findAll(int page, int size) {
    var pageable = PageRequest.of(page, size);
    var list = repository.findAllByOrderById(pageable)
                         .map(converter::convertToDto)
                         .collectList();
    var count = repository.count();
    return list.zipWith(count, (entities, totalElements) -> new PageImpl<>(entities, pageable, totalElements));
  }

  public Mono<ProductModel> find(int id) {
    return repository.findById(id)
                     .map(converter::convertToDto);
  }

}
