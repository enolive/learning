package de.welcz.camundawebflux

import net.bytebuddy.utility.RandomString

class ObjectMother {
  static aString() {
    RandomString.make(42)
  }
}
