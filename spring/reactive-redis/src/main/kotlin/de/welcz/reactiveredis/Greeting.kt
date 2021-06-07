package de.welcz.reactiveredis

import java.io.Serializable

data class Greeting(val name: String) : Serializable {
  val text = "Hello, $name!"
}
