package de.welcz.kotestwithtestcontainers

import jakarta.persistence.Column
import jakarta.persistence.Entity
import jakarta.persistence.Id
import jakarta.persistence.Table
import org.hibernate.annotations.UuidGenerator
import org.springframework.data.repository.CrudRepository
import java.math.BigDecimal
import java.util.*

interface BeerRepository : CrudRepository<Beer, UUID>

@Entity
@Table(name = "beers")
data class Beer(
  @Id
  @UuidGenerator
  @Column(name = "beer_id")
  val id: UUID? = null,
  val brand: String,
  val name: String,
  val strength: BigDecimal,
)
