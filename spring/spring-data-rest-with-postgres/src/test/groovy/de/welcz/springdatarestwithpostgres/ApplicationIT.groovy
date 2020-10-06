package de.welcz.springdatarestwithpostgres

import org.junit.jupiter.api.Tag
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.autoconfigure.ImportAutoConfiguration
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureWebMvc
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.http.MediaType
import org.springframework.test.web.servlet.MockMvc
import spock.lang.Specification
import spock.lang.Stepwise

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status

@SpringBootTest
@AutoConfigureWebMvc
@AutoConfigureMockMvc
@ImportAutoConfiguration
@Stepwise
class ApplicationIT extends Specification {
  @Autowired
  private MockMvc mockMvc

  def "product can be created"() {
    given: "a product"
    def product = """{
      "name": "Fist of Adionis"
    }"""

    when: "POST request is performed"
    def response = mockMvc.perform(post("/api/v1/products")
        .contentType(MediaType.APPLICATION_JSON)
        .content(product)
    )

    then: "status is created"
    response.andExpect(status().isCreated())
    and: "location of created resource is returned"
    response.andExpect(header().string("Location", "http://localhost/api/v1/products/1"))
  }

  def "product can be modified"() {
    given: "a modified product"
    def product = """{
      "name": "Fist of Adonis"
    }"""

    when: "PUT request is performed"
    def response = mockMvc.perform(put("/api/v1/products/1")
        .contentType(MediaType.APPLICATION_JSON)
        .content(product)
    )

    then: "status is no content"
    response.andExpect(status().isNoContent())
  }

  def "product can be deleted"() {
    when: "DELETE request is performed"
    def response = mockMvc.perform(delete("/api/v1/products/1"))

    then: "status is no content"
    response.andExpect(status().isNoContent())
  }
}
