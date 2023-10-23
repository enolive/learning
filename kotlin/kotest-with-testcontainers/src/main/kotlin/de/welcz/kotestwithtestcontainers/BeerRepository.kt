package de.welcz.kotestwithtestcontainers

import org.springframework.data.annotation.Id
import org.springframework.data.relational.core.mapping.Column
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.math.BigDecimal
import java.util.*

interface BeerRepository : CoroutineCrudRepository<Beer, UUID>

@Table(name = "beers")
data class Beer(
  @Id
  @Column("beer_id")
  val id: UUID?,
  val brand: String,
  val name: String,
  val strength: BigDecimal,
)
