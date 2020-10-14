package de.welcz.r2dbc.converter

import de.welcz.r2dbc.api.ModifyProduct
import de.welcz.r2dbc.api.PatchProduct
import de.welcz.r2dbc.api.ProductModel
import de.welcz.r2dbc.persistence.ProductEntity
import spock.lang.Shared
import spock.lang.Specification

class ProductConverterTest extends Specification {
  @Shared
  private ProductConverter sut = new ProductConverter()

  def "convert to entity works"() {
    given: "a product to modify"
    def productToModify = new ModifyProduct("Description", 56.7)
    and: "an expected entity"
    def expectedEntity = new ProductEntity(null, "Description", 56.7)

    when: "product is converted"
    def result = sut.convertToEntity(productToModify)

    then: "expected entity is returned"
    result == expectedEntity
  }

  def "convert to dto works"() {
    given: "an existing entity"
    def existingEntity = new ProductEntity(67, "Description", 56.7)
    and: "an expected product"
    def expectedProduct = new ProductModel(67, "Description", 56.7)

    when: "entity is converted"
    def result = sut.convertToDto(existingEntity)

    then: "expected entity is returned"
    result == expectedProduct
  }

  def "patch existing entity works"() {
    given: "an existing entity"
    def existingEntity = new ProductEntity(67, "Description", 56.7)

    when: "entity is patched"
    def result = sut.patchExistingEntity(existingEntity, patch)

    then: "expected entity is returned"
    result == expectedEntity

    where:
    patch                                     | expectedEntity
    new PatchProduct(null, null)              | new ProductEntity(67, "Description", 56.7)
    new PatchProduct(null, 99.6)              | new ProductEntity(67, "Description", 99.6)
    new PatchProduct("New Description", null) | new ProductEntity(67, "New Description", 56.7)
    new PatchProduct("New Description", 99.6) | new ProductEntity(67, "New Description", 99.6)
  }

  def "convert to entity with id works"() {
    given: "a product to modify"
    def productToModify = new ModifyProduct("Description", 56.7)
    and: "a product id"
    def productId = 42
    and: "an expected entity"
    def expectedEntity = new ProductEntity(42, "Description", 56.7)

    when: "product is converted"
    def result = sut.convertToEntityWithId(productId).apply(productToModify)

    then: "expected entity is returned"
    result == expectedEntity
  }
}
