package de.welcz.r2dbc.api

import de.welcz.r2dbc.persistence.ProductDao
import org.spockframework.spring.SpringBean
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.web.reactive.WebFluxTest
import org.springframework.data.domain.PageImpl
import org.springframework.data.domain.PageRequest
import org.springframework.hateoas.config.EnableHypermediaSupport
import org.springframework.http.MediaType
import org.springframework.test.web.reactive.server.WebTestClient
import reactor.core.publisher.Flux
import reactor.core.publisher.Mono
import spock.lang.Specification

@WebFluxTest(controllers = ProductController)
@EnableHypermediaSupport(type = EnableHypermediaSupport.HypermediaType.HAL)
class ProductControllerTest extends Specification {
  @Autowired
  private WebTestClient testClient
  @SpringBean
  private ProductDao dao = Mock()

  def "find all works"() {
    given: "an expected response"
    // language=JSON
    def expected = """[
      {
        "description": "Awesome",
        "price": 99.5,
        "_links": {
          "self": {
            "href": "/products/42"
          }
        }
      }
    ]"""

    when: "GET /products is called"
    def response = testClient.get()
                             .uri("/products")
                             .exchange()

    then: "status is ok"
    response.expectStatus().isOk()
    and: "body contains the expected response"
    response.expectBody().json(expected)
    and: "dao returns some products"
    1 * dao.findAll() >> Flux.fromIterable([new ProductModel(42, "Awesome", 99.5)])
  }

  def "find all with pagination works"() {
    given: "page parameters"
    def page = 2
    def limit = 10
    and: "a page response"
    def pageResponse = new PageImpl<>(
        [new ProductModel(42, "Awesome", 99.5)],
        PageRequest.of(page, limit),
        123)
    and: "an expected response"
    // language=JSON
    def expected = """{
      "_embedded": {
        "productModelList": [
          {
            "description": "Awesome",
            "price": 99.5,
            "_links": {
              "self": {
                "href": "/products/42"
              }
            }
          }
        ]
      },
      "_links": {
        "first": {
          "href": "/products/paged?page=0&limit=10"
        },
        "prev": {
          "href": "/products/paged?page=1&limit=10"
        },
        "next": {
          "href": "/products/paged?page=3&limit=10"
        },
        "last": {
          "href": "/products/paged?page=12&limit=10"
        }
      },
      "page": {
        "size": 10,
        "totalElements": 123,
        "totalPages": 13,
        "number": 2
      }
    }"""

    when: "GET /products is called"
    def response = testClient.get()
                             .uri("/products/paged?page=$page&limit=$limit")
                             .exchange()

    then: "status is ok"
    response.expectStatus().isOk()
    and: "body contains the expected response"
    response.expectBody().json(expected)
    and: "dao returns some products"
    1 * dao.findAll(page, limit) >> Mono.just(pageResponse)
  }

  def "find specific works"() {
    given: "a product id"
    def productId = 42
    and: "an expected response"
    // language=JSON
    def expected = """
      {
        "description": "Awesome",
        "price": 99.5,
        "_links": {
          "self": {
            "href": "/products/42"
          }
        }
      }
    """

    when: "GET /products/{id} is called"
    def response = testClient.get()
                             .uri("/products/$productId")
                             .exchange()

    then: "status is ok"
    response.expectStatus().isOk()
    and: "body contains the expected response"
    response.expectBody().json(expected)
    and: "dao returns a product"
    1 * dao.find(productId) >> Mono.just(new ProductModel(42, "Awesome", 99.5))
  }

  def "create works"() {
    given: "a product"
    // language=JSON
    def product = """
    {
      "description": "Awesome",
      "price": 99.5
    }
    """
    and: "a product to modify"
    def productToModify = new ModifyProduct("Awesome", 99.5)
    and: "a saved product"
    def savedProduct = new ProductModel(42, "Awesome", 99.5)
    and: "an expected response"
    // language=JSON
    def expected = """
      {
        "description": "Awesome",
        "price": 99.5,
        "_links": {
          "self": {
            "href": "/products/42"
          }
        }
      }
    """

    when: "POST /products is called"
    def response = testClient.post()
                             .uri("/products")
                             .contentType(MediaType.APPLICATION_JSON)
                             .bodyValue(product)
                             .exchange()

    then: "status is ok"
    response.expectStatus().isOk()
    and: "body contains the expected response"
    response.expectBody().json(expected)
    and: "dao creates product"
    1 * dao.create(productToModify) >> Mono.just(savedProduct)
  }

  def "update total works"() {
    given: "a product"
    // language=JSON
    def product = """
    {
      "description": "Awesome",
      "price": 99.5
    }
    """
    and: "a product id"
    def productId = 42
    and: "a product to modify"
    def productToModify = new ModifyProduct("Awesome", 99.5)
    and: "a saved product"
    def savedProduct = new ProductModel(42, "Awesome", 99.5)
    and: "an expected response"
    // language=JSON
    def expected = """
      {
        "description": "Awesome",
        "price": 99.5,
        "_links": {
          "self": {
            "href": "/products/42"
          }
        }
      }
    """

    when: "PUT /products/{id} is called"
    def response = testClient.put()
                             .uri("/products/$productId")
                             .contentType(MediaType.APPLICATION_JSON)
                             .bodyValue(product)
                             .exchange()

    then: "status is ok"
    response.expectStatus().isOk()
    and: "body contains the expected response"
    response.expectBody().json(expected)
    and: "dao updates product"
    1 * dao.updateTotal(productId, productToModify) >> Mono.just(savedProduct)
  }

  def "update total returns no content"() {
    given: "a product"
    // language=JSON
    def product = """
    {
      "description": "Awesome",
      "price": 99.5
    }
    """
    and: "a product id"
    def productId = 42
    when: "PUT /products/{id} is called"
    def response = testClient.put()
                             .uri("/products/$productId")
                             .contentType(MediaType.APPLICATION_JSON)
                             .bodyValue(product)
                             .exchange()

    then: "status is no content"
    response.expectStatus().isNoContent()
    and: "dao returns empty"
    1 * dao.updateTotal(*_) >> Mono.empty()
  }

  def "update partial works"() {
    given: "a product"
    // language=JSON
    def product = """
    {
      "description": "Awesome",
      "price": 99.5
    }
    """
    and: "a product id"
    def productId = 42
    and: "a product to modify"
    def productToModify = new PatchProduct("Awesome", 99.5)
    and: "a saved product"
    def savedProduct = new ProductModel(42, "Awesome", 99.5)
    and: "an expected response"
    // language=JSON
    def expected = """
      {
        "description": "Awesome",
        "price": 99.5,
        "_links": {
          "self": {
            "href": "/products/42"
          }
        }
      }
    """

    when: "PATCH /products/{id} is called"
    def response = testClient.patch()
                             .uri("/products/$productId")
                             .contentType(MediaType.APPLICATION_JSON)
                             .bodyValue(product)
                             .exchange()

    then: "status is ok"
    response.expectStatus().isOk()
    and: "body contains the expected response"
    response.expectBody().json(expected)
    and: "dao updates product"
    1 * dao.updatePartial(productId, productToModify) >> Mono.just(savedProduct)
  }

  def "update partial returns no content"() {
    given: "a product"
    // language=JSON
    def product = """
    {
      "description": "Awesome",
      "price": 99.5
    }
    """
    and: "a product id"
    def productId = 42

    when: "PATCH /products/{id} is called"
    def response = testClient.patch()
                             .uri("/products/$productId")
                             .contentType(MediaType.APPLICATION_JSON)
                             .bodyValue(product)
                             .exchange()

    then: "status is no content"
    response.expectStatus().isNoContent()
    and: "dao returns empty"
    1 * dao.updatePartial(*_) >> Mono.empty()
  }

  def "delete works"() {
    given: "a product id"
    def productId = 42

    when: "DELETE /products/{id} is called"
    def response = testClient.delete()
                             .uri("/products/$productId")
                             .exchange()

    then: "status is no content"
    response.expectStatus().isNoContent()
    and: "dao deleted product"
    1 * dao.delete(productId) >> Mono.empty()
  }

  def "find specific returns no content"() {
    given: "a product id"
    def productId = 42

    when: "GET /products/{id} is called"
    def response = testClient.get()
                             .uri("/products/$productId")
                             .exchange()

    then: "status is no content"
    response.expectStatus().isNoContent()
    and: "dao returns empty"
    1 * dao.find(productId) >> Mono.empty()
  }
}
