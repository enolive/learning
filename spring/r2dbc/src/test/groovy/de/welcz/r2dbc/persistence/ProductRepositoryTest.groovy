package de.welcz.r2dbc.persistence

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.data.r2dbc.DataR2dbcTest
import org.springframework.context.annotation.Import
import org.springframework.data.domain.Pageable
import org.springframework.test.context.ActiveProfiles
import spock.lang.Specification

@DataR2dbcTest
@ActiveProfiles("testdb")
@Import(MemoryDBConfiguration)
class ProductRepositoryTest extends Specification {
  @Autowired
  private ProductRepository sut

  def "find works"() {
    given: "some existing products"
    def products = [
        new ProductEntity(description: "Product 1", price: 12.3),
        new ProductEntity(description: "Product 2", price: 67.8),
        new ProductEntity(description: "Product 3", price: 0.0),
    ]
    sut.saveAll(products).blockLast()

    when: "list of products is queried"
    def result = sut.findAllByOrderById(Pageable.unpaged())
                    .collectList()
                    .block()

    then: "the expected result is retrieved"
    result == products
  }
}
