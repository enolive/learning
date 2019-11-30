package de.welcz.multipartapidocs

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.web.reactive.WebFluxTest
import org.springframework.core.io.ClassPathResource
import org.springframework.http.MediaType
import org.springframework.http.client.MultipartBodyBuilder
import org.springframework.test.web.reactive.server.WebTestClient
import org.springframework.web.reactive.function.BodyInserters
import spock.lang.Specification

@WebFluxTest(controllers = Controller)
class ControllerTest extends Specification {
  @Autowired
  private WebTestClient testClient

  def "upload works"() {
    given: "an uri"
    def uri = "/uploads"
    and: "a file to upload"
    def builder = new MultipartBodyBuilder()
    builder.part("file",
                 new ClassPathResource("test_image.png"),
                 MediaType.IMAGE_PNG)
           .filename("my_file.png")
    def multiValueMap = builder.build()

    when: "api is called"
    def response = testClient.post()
                             .uri(uri)
                             .body(BodyInserters.fromMultipartData(multiValueMap))
                             .exchange()

    then: "status is created"
    response.expectStatus().isCreated()
    and: "response says that the file was uploaded"
    response.expectBody(String).isEqualTo("uploaded my_file.png of type image/png")
  }
}
