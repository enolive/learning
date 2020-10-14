package de.welcz.r2dbc

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.core.env.Environment
import spock.lang.Specification

@SpringBootTest
class ApplicationTest extends Specification {
  @Autowired
  private Environment environment

  def "application loads"() {
    expect: "context is present"
    environment != null
  }
}
