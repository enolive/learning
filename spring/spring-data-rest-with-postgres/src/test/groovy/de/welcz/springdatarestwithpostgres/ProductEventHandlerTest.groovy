package de.welcz.springdatarestwithpostgres

import org.springframework.http.HttpStatus
import org.springframework.web.server.ResponseStatusException
import spock.lang.Shared
import spock.lang.Specification

class ProductEventHandlerTest extends Specification {
  @Shared
  private ProductEventHandler sut = new ProductEventHandler()

  def "save with bad words is rejected"() {
    given: "a product containing a bad word"
    def product = new Product(name: "Fibu")

    when: "product is about to be saved"
    sut.onSave(product)

    then: "save is rejected"
    def exception = thrown(ResponseStatusException)
    exception.status == HttpStatus.BAD_REQUEST
    exception.reason == "Don't say such nasty words"
  }

  def "save succeeds"() {
    given: "a product containing"
    def product = new Product(name: name)

    when: "product is about to be saved"
    sut.onSave(product)

    then: "save is accepted"
    noExceptionThrown()
    where:
    name << ["Test", "My Product", "Something else"]
  }
}
