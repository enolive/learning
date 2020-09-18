package de.welcz.r2dbc;

import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.Objects;

@RestController
public class ProductController {
  private final ProductRepository repository;

  public ProductController(ProductRepository repository) {
    this.repository = repository;
  }

  @PostMapping("/products")
  public Mono<Product> upsertProduct(@RequestBody Mono<Product> product) {
    return product.filter(toSave -> toSave.getId() != null)
                  .flatMap(this::updateExisting)
                  .switchIfEmpty(createNew(product));
  }

  @DeleteMapping("/products/{id}")
  public Mono<Void> deleteProduct(@PathVariable int id) {
    return repository.deleteById(id);
  }

  @GetMapping("/products")
  public Flux<Product> findAllProducts() {
    return repository.findAllByOrderById();
  }

  private Mono<Product> createNew(Mono<Product> product) {
    return product.map(Product::setAsNew)
                  .flatMap(repository::save);
  }

  private Mono<Product> updateExisting(Product update) {
    return repository.findById(Objects.requireNonNull(update.getId()))
                     .flatMap(existing -> {
                       existing.setDescription(update.getDescription());
                       existing.setPrice(update.getPrice());
                       return repository.save(existing);
                     });
  }
}
