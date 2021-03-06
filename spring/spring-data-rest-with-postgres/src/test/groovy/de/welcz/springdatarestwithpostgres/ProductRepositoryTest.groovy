package de.welcz.springdatarestwithpostgres

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest
import spock.lang.Specification

@DataJpaTest
class ProductRepositoryTest extends Specification {
  @Autowired
  private ProductRepository sut

  def "find by name works"() {
    given: "existing products"
    def product1 = new Product(name: "Fist of Adonis")
    def product2 = new Product(name: "Search Test")
    def product3 = new Product(name: "Something Test")
    sut.saveAll([product1, product2, product3])

    when: "list of products is queried"
    def result = sut.findProductsByNameContaining("Test")

    then: "result contains the expected hit"
    result.asJava() == [product2, product3]
  }

  def "find by id works"() {
    given: "an existing product"
    def product = new Product(name: "Fist of Adonis")
    def existing = sut.save(product)

    when: "product is looked up"
    def result = sut.findById(existing.id)

    then: "product is found"
    result.defined
  }
}
