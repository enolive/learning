package de.welcz.r2dbc.persistence

import de.welcz.r2dbc.api.ModifyProduct
import de.welcz.r2dbc.api.PatchProduct
import de.welcz.r2dbc.api.ProductModel
import de.welcz.r2dbc.converter.ProductConverter
import org.spockframework.spring.SpringBean
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.data.domain.PageImpl
import org.springframework.data.domain.PageRequest
import org.springframework.data.domain.Pageable
import org.springframework.test.context.ContextConfiguration
import reactor.core.publisher.Flux
import reactor.core.publisher.Mono
import spock.lang.Specification

@SpringBootTest
@ContextConfiguration(classes = ProductDao)
class ProductDaoTest extends Specification {
  @Autowired
  private ProductDao sut
  @SpringBean
  private ProductRepository repository = Mock()
  @SpringBean
  private ProductConverter converter = Mock()

  def "find all works"() {
    given: "existing products in the database"
    def product1 = new ProductEntity(description: "Product 1")
    def product2 = new ProductEntity(description: "Product 2")
    def product3 = new ProductEntity(description: "Product 3")
    def existingProducts = [product1, product2, product3]
    and: "converted entities"
    def productDto1 = new ProductModel(1, "Product 1", 1.0)
    def productDto2 = new ProductModel(2, "Product 2", 2.0)
    def productDto3 = new ProductModel(3, "Product 3", 3.0)

    when: "list of products is queried"
    def products = sut.findAll().collectList().block()

    then: "expected products are returned"
    products == [productDto1, productDto2, productDto3]
    and: "products were found in repository"
    1 * repository.findAllByOrderById(Pageable.unpaged()) >> Flux.fromIterable(existingProducts)
    and: "products were converted to dtos"
    1 * converter.convertToDto(product1) >> productDto1
    1 * converter.convertToDto(product2) >> productDto2
    1 * converter.convertToDto(product3) >> productDto3
  }

  def "find all with paging works"() {
    given: "page parameters"
    def page = 2
    def limit = 42
    and: "existing products in the database"
    def product1 = new ProductEntity(description: "Product 1")
    def existingProducts = [product1]
    and: "converted entities"
    def productDto1 = new ProductModel(1, "Product 1", 1.0)
    and: "expected page"
    def totalItems = 1234L
    def expectedPageRequest = PageRequest.of(page, limit)
    def expectedPage = new PageImpl([productDto1], expectedPageRequest, totalItems)

    when: "page of products is queried"
    def productsPage = sut.findAll(page, limit).block()

    then: "expected products are returned"
    productsPage == expectedPage
    and: "products were found in repository"
    1 * repository.findAllByOrderById(expectedPageRequest) >> Flux.fromIterable(existingProducts)
    and: "total number of products was queried"
    1 * repository.count() >> Mono.just(totalItems)
    and: "products were converted to dtos"
    1 * converter.convertToDto(product1) >> productDto1
  }

  def "find specific works"() {
    given: "a product id"
    def productId = 42
    and: "existing products in the database"
    def existingProduct = new ProductEntity(description: "Product 1")
    and: "converted entity"
    def expectedProduct = new ProductModel(1, "Product 1", 1.0)

    when: "single product is queried"
    def products = sut.find(productId).block()

    then: "expected product are returned"
    products == expectedProduct
    and: "product was found in repository"
    1 * repository.findById(productId) >> Mono.just(existingProduct)
    and: "product was converted to dto"
    1 * converter.convertToDto(existingProduct) >> expectedProduct
  }

  def "create works"() {
    given: "a product to create"
    def product = new ModifyProduct("Awesome", 42.1)
    and: "an expected product"
    def expectedProduct = new ProductModel(76, "Awesome", 42.1)
    and: "an entity to be saved"
    def entityToBeSaved = new ProductEntity(null, "Awesome", 42.1)
    and: "a saved entity"
    def savedEntity = new ProductEntity(76, "Awesome", 42.1)

    when: "product is created"
    def result = sut.create(product).block()

    then: "the created product is returned"
    result == expectedProduct
    and: "product was converted to entity"
    1 * converter.convertToEntity(product) >> entityToBeSaved
    and: "entity was saved"
    1 * repository.save(entityToBeSaved) >> Mono.just(savedEntity)
    and: "saved entity was converted to dto"
    1 * converter.convertToDto(savedEntity) >> expectedProduct
  }

  def "update totally works"() {
    given: "a product to modify"
    def product = new ModifyProduct("Changed", 42.1)
    and: "a product id"
    def productId = 76
    and: "an expected product"
    def expectedProduct = new ProductModel(76, "Awesome", 42.1)
    and: "an entity to be saved"
    def entityToBeSaved = new ProductEntity(1, "Awesome", 42.1)
    and: "a saved entity"
    def savedEntity = new ProductEntity(2, "Awesome", 42.1)

    when: "product is created"
    def result = sut.updateTotal(productId, product).block()

    then: "the created product is returned"
    result == expectedProduct
    and: "product was converted to entity"
    1 * converter.convertToEntityWithId(productId) >> {
      return { _ -> entityToBeSaved }
    }
    and: "entity was saved"
    1 * repository.save(entityToBeSaved) >> Mono.just(savedEntity)
    and: "saved entity was converted to dto"
    1 * converter.convertToDto(savedEntity) >> expectedProduct
  }

  def "update partially works"() {
    given: "a product id"
    def productId = 76
    and: "a product to modifiy"
    def productToModify = new PatchProduct("Awesome", 42.1)
    and: "an existing product entity"
    def existingEntity = new ProductEntity(5, "Existing", 99.0)
    and: "an expected product"
    def expectedProduct = new ProductModel(76, "Awesome", 42.1)
    and: "an entity to be saved"
    def entityToBeSaved = new ProductEntity(2, "Awesome", 42.1)
    and: "a saved entity"
    def savedEntity = new ProductEntity(2, "Awesome", 42.1)

    when: "product is created"
    def result = sut.updatePartial(productId, productToModify).block()

    then: "the created product is returned"
    result == expectedProduct
    and: "existing entity was found"
    1 * repository.findById(productId) >> Mono.just(existingEntity)
    and: "existing entity was patched"
    1 * converter.patchExistingEntity(existingEntity, productToModify) >> entityToBeSaved
    and: "entity was saved"
    1 * repository.save(entityToBeSaved) >> Mono.just(savedEntity)
    and: "saved entity was converted to dto"
    1 * converter.convertToDto(savedEntity) >> expectedProduct
  }

  def "delete works"() {
    given: "a product id"
    def productId = 42

    when: "existing product is deleted"
    sut.delete(productId).block()

    then: "product was deleted in repository"
    1 * repository.deleteById(42) >> Mono.empty()
  }
}
