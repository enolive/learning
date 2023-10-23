package de.welcz.kotestwithtestcontainers

import org.testcontainers.containers.PostgreSQLContainer
import org.testcontainers.utility.DockerImageName

object TestDatabase {
  init {
    val container: PostgreSQLContainer<*> =
      PostgreSQLContainer<Nothing>(DockerImageName.parse("postgres:16"))
    container.start()
    registerDbProps(container)
  }

  fun start() {
  }

  private fun registerDbProps(container: PostgreSQLContainer<*>) {
    System.setProperty("spring.r2dbc.url", r2dbcUrl(container))
    System.setProperty("spring.r2dbc.username", container.username)
    System.setProperty("spring.r2dbc.password", container.password)
    System.setProperty("spring.flyway.url", container.getJdbcUrl())
    System.setProperty("spring.flyway.user", container.username)
    System.setProperty("spring.flyway.password", container.password)
  }

  private fun r2dbcUrl(container: PostgreSQLContainer<*>) =
    "r2dbc:postgresql://${container.host}:${container.getMappedPort(PostgreSQLContainer.POSTGRESQL_PORT)}/${container.databaseName}"
}
