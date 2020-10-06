package de.welcz.springdatarestwithpostgres;

import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;
import org.springframework.data.rest.core.annotation.RestResource;

@RepositoryRestResource(collectionResourceRel = "products", path = "products")
public interface ProductRepository extends PagingAndSortingRepository<Product, Long> {
  @RestResource(path = "by-name", rel = "by-name")
  Iterable<Product> findProductsByNameContaining(String name);
}
