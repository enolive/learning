package de.welcz.r2dbc.api;

import de.welcz.r2dbc.persistence.ProductDao;
import org.springframework.hateoas.server.reactive.WebFluxLinkBuilder;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import javax.validation.Valid;

import static org.springframework.hateoas.server.reactive.WebFluxLinkBuilder.linkTo;
import static org.springframework.hateoas.server.reactive.WebFluxLinkBuilder.methodOn;

@RestController
public class ProductController {
  private final ProductDao dao;

  public ProductController(ProductDao dao) {
    this.dao = dao;
  }

  @GetMapping("/products")
  public Flux<ProductModel> findAllProducts() {
    return dao.findAll()
              .flatMap(Links::addSelfRel);
  }

  @GetMapping("/products/{id}")
  public Mono<ProductModel> showProduct(@PathVariable int id) {
    return dao.find(id)
              .flatMap(Links::addSelfRel)
              .switchIfEmpty(Responses.noContent());
  }

  @PostMapping("/products")
  public Mono<ProductModel> createProduct(@RequestBody @Valid Mono<ModifyProduct> product) {
    return dao.create(product)
              .flatMap(Links::addSelfRel);
  }

  @PutMapping("/products/{id}")
  public Mono<ProductModel> updateProductTotal(@PathVariable int id, @RequestBody @Valid Mono<ModifyProduct> product) {
    return dao.updateTotal(id, product)
              .flatMap(Links::addSelfRel)
              .switchIfEmpty(Responses.noContent());
  }

  @PatchMapping("/products/{id}")
  public Mono<ProductModel> updateProductPartial(@PathVariable int id, @RequestBody @Valid Mono<PatchProduct> product) {
    return dao.updatePartial(id, product)
              .flatMap(Links::addSelfRel)
              .switchIfEmpty(Responses.noContent());
  }

  @DeleteMapping("/products/{id}")
  @ResponseStatus(HttpStatus.NO_CONTENT)
  public Mono<Void> deleteProduct(@PathVariable int id) {
    return dao.delete(id);
  }

  private static class Responses {
    private static Mono<? extends ProductModel> noContent() {
      return Mono.error(new ResponseStatusException(HttpStatus.NO_CONTENT));
    }
  }

  private static class Links {
    private static Mono<ProductModel> addSelfRel(ProductModel product) {
      return linkToProduct(product).withSelfRel()
                                   .toMono()
                                   // XXX: due to the lack of monadic comprehensions, we need a mapper here
                                   .map(product::add);
    }

    private static WebFluxLinkBuilder.WebFluxBuilder linkToProduct(ProductModel product) {
      return linkTo(methodOn(ProductController.class).showProduct(product.getId()));
    }
  }
}
