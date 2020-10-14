package de.welcz.r2dbc.persistence;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.data.r2dbc.DataR2dbcTest;
import org.springframework.context.annotation.Import;
import org.springframework.test.context.ActiveProfiles;

import java.util.List;

@DataR2dbcTest
@ActiveProfiles("testdb")
@Import(MemoryDBConfiguration.class)
class ProductRepositoryTest {
  @Autowired
  private ProductRepository sut;

  @Test
  void saveWorks() {
    sut.saveAll(List.of(new ProductEntity(null, "My Awesome Product", 125.23)))
       .collectList()
       .block();
  }
}