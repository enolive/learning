package de.welcz.r2dbc.api;

import de.welcz.r2dbc.persistence.ProductDao;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.hateoas.IanaLinkRelations;
import org.springframework.hateoas.Link;
import org.springframework.hateoas.PagedModel;
import org.springframework.hateoas.server.reactive.WebFluxLinkBuilder;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import javax.validation.Valid;
import java.util.List;

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

  @GetMapping("/products/paged")
  public Mono<PagedModel<ProductModel>> findPagedProducts(
      @RequestParam(required = false, defaultValue = "0") int page,
      @RequestParam(required = false, defaultValue = "20") int limit) {
    return dao.findAll(page, limit)
              .flatMap(Links::addSelfRelToEachProduct)
              .flatMap(Links::addPagination);
  }

  @GetMapping("/products/{id}")
  public Mono<ProductModel> showProduct(@PathVariable int id) {
    return dao.find(id)
              .flatMap(Links::addSelfRel)
              .switchIfEmpty(Responses.noContent());
  }

  @PostMapping("/products")
  public Mono<ProductModel> createProduct(@RequestBody @Valid ModifyProduct product) {
    return dao.create(product)
              .flatMap(Links::addSelfRel);
  }

  @PutMapping("/products/{id}")
  public Mono<ProductModel> updateProductTotal(@PathVariable int id, @RequestBody @Valid ModifyProduct product) {
    return dao.updateTotal(id, product)
              .flatMap(Links::addSelfRel)
              .switchIfEmpty(Responses.noContent());
  }

  @PatchMapping("/products/{id}")
  public Mono<ProductModel> updateProductPartial(@PathVariable int id, @RequestBody @Valid PatchProduct product) {
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
    private Responses() {
    }

    private static Mono<? extends ProductModel> noContent() {
      return Mono.error(new ResponseStatusException(HttpStatus.NO_CONTENT));
    }
  }

  static class Links {
    private Links() {
    }

    public static Mono<Page<ProductModel>> addSelfRelToEachProduct(Page<ProductModel> page) {
      return Flux.fromIterable(page.getContent())
                 .flatMap(Links::addSelfRel)
                 .collectList()
                 .map(products -> new PageImpl<>(products, page.getPageable(), page.getTotalElements()));
    }

    public static Mono<PagedModel<ProductModel>> addPagination(Page<ProductModel> page) {
      return createPaginationLinks(page).map(ls -> PagedModel.of(page.getContent(),
                                                                 metadataFrom(page),
                                                                 ls));
    }

    public static Mono<ProductModel> addSelfRel(ProductModel product) {
      return linkToProduct(product).withSelfRel()
                                   .toMono()
                                   .map(product::add);
    }

    private static PagedModel.PageMetadata metadataFrom(Page<ProductModel> page) {
      return new PagedModel.PageMetadata(page.getSize(), page.getNumber(), page.getTotalElements());
    }

    private static Mono<List<Link>> createPaginationLinks(Page<ProductModel> page) {
      var size = page.getSize();
      var number = page.getNumber();
      var totalPages = page.getTotalPages();
      var firstLink = linkToPage(0, size)
          .withRel(IanaLinkRelations.FIRST)
          .toMono();
      var prevLink = linkToPage(number - 1, size)
          .withRel(IanaLinkRelations.PREV)
          .toMono()
          .filter(link -> number > 0);
      var nextLink = linkToPage(number + 1, size)
          .withRel(IanaLinkRelations.NEXT)
          .toMono()
          .filter(it -> number < totalPages - 1);
      var lastLink = linkToPage(totalPages - 1, size)
          .withRel(IanaLinkRelations.LAST)
          .toMono();
      return Flux.concat(firstLink, prevLink, nextLink, lastLink)
                 .collectList();
    }

    private static WebFluxLinkBuilder.WebFluxBuilder linkToPage(int number, int size) {
      return linkTo(methodOn(ProductController.class).findPagedProducts(number, size));
    }

    private static WebFluxLinkBuilder.WebFluxBuilder linkToProduct(ProductModel product) {
      return linkTo(methodOn(ProductController.class).showProduct(product.getId()));
    }
  }
}
