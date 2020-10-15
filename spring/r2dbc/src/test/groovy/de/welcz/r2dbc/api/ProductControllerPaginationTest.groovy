package de.welcz.r2dbc.api

import org.springframework.data.domain.PageImpl
import org.springframework.data.domain.PageRequest
import org.springframework.hateoas.PagedModel
import spock.lang.Specification

class ProductControllerPaginationTest extends Specification {
  def "pagination is correctly added"() {
    given: "a page"
    def page = new PageImpl<ProductModel>(
        [
            new ProductModel(1, "First", 1.0),
            new ProductModel(2, "Second", 2.0),
            new ProductModel(3, "Third", 3.0)
        ],
        PageRequest.of(number, 1),
        3
    )
    and: "expected metadata"
    def expectedMetadata = new PagedModel.PageMetadata(1, number, 3)

    when: "page is added"
    def result = ProductController.Links.addPagination(page).block()

    then: "expected result is retrieved"
    result.metadata == expectedMetadata
    result.links.toList().collect { it.toString() } == expectedLinks

    where:
    number | expectedLinks
    1      | [
        '</products/paged?page=0&limit=1>;rel="first"',
        '</products/paged?page=0&limit=1>;rel="prev"',
        '</products/paged?page=2&limit=1>;rel="next"',
        '</products/paged?page=2&limit=1>;rel="last"',
    ]
    0      | [
        '</products/paged?page=0&limit=1>;rel="first"',
        '</products/paged?page=1&limit=1>;rel="next"',
        '</products/paged?page=2&limit=1>;rel="last"',
    ]
    2      | [
        '</products/paged?page=0&limit=1>;rel="first"',
        '</products/paged?page=1&limit=1>;rel="prev"',
        '</products/paged?page=2&limit=1>;rel="last"',
    ]
  }
}
