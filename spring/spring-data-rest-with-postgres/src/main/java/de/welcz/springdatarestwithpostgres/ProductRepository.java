package de.welcz.springdatarestwithpostgres;

import io.vavr.collection.Seq;
import io.vavr.control.Option;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;
import org.springframework.data.rest.core.annotation.RestResource;

@RepositoryRestResource(collectionResourceRel = "products", path = "products")
public interface ProductRepository extends PagingAndSortingRepository<Product, Long> {
  @RestResource(path = "by-name", rel = "by-name")
  Seq<Product> findProductsByNameContaining(String name);

  Option<Product> findById(long id);
}
