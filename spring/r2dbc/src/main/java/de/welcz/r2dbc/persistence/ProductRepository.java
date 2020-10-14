package de.welcz.r2dbc.persistence;

import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;

@Repository
public interface ProductRepository extends ReactiveCrudRepository<ProductEntity, Integer> {
  Flux<ProductEntity> findAllByOrderById(Pageable pageable);
}
