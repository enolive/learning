package de.welcz.r2dbc.api;

import de.welcz.r2dbc.persistence.ProductDao;
import org.springframework.hateoas.Link;
import org.springframework.hateoas.RepresentationModel;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import javax.validation.Valid;
import java.util.function.Function;

import static org.springframework.hateoas.server.reactive.WebFluxLinkBuilder.linkTo;
import static org.springframework.hateoas.server.reactive.WebFluxLinkBuilder.methodOn;

@RestController
public class ProductController {
  private final ProductDao dao;

  public ProductController(ProductDao dao) {
    this.dao = dao;
  }

  @GetMapping("/products/{id}")
  public Mono<ProductModel> showProduct(@PathVariable int id) {
    return dao.find(id)
              .zipWith(productLink(id), RepresentationModel::add)
              .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.NO_CONTENT)));
  }

  @PostMapping("/products")
  public Mono<ProductModel> createProduct(@RequestBody @Valid Mono<ModifyProduct> product) {
    return dao.create(product)
              .zipWhen(productLink(), RepresentationModel::add);
  }

  @PutMapping("/products/{id}")
  public Mono<ProductModel> updateProduct(@RequestBody @Valid Mono<ModifyProduct> product, @PathVariable int id) {
    return dao.update(id, product)
              .zipWith(productLink(id), RepresentationModel::add)
              .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.NO_CONTENT)));
  }

  @DeleteMapping("/products/{id}")
  @ResponseStatus(HttpStatus.NO_CONTENT)
  public Mono<Void> deleteProduct(@PathVariable int id) {
    return dao.delete(id);
  }

  @GetMapping("/products")
  public Flux<ProductModel> findAllProducts() {
    return dao.findAll()
              .flatMap(product -> productLink(product.getId()).map(product::add));
  }

  private Function<ProductModel, Mono<? extends Link>> productLink() {
    return created -> productLink(created.getId());
  }

  private Mono<Link> productLink(int id) {
    var controller = ProductController.class;
    return linkTo(methodOn(controller).showProduct(id)).withSelfRel().toMono();
  }
}
