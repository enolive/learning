plugins {
  kotlin("jvm") version "1.5.10"
}

val kotest_version: String by project

tasks.withType<Test> {
  useJUnitPlatform()
}

group = "de.welcz"
version = "1.0-SNAPSHOT"

repositories {
  mavenCentral()
}

dependencies {
  implementation(kotlin("stdlib"))
  testImplementation("io.kotest:kotest-runner-junit5:$kotest_version")
  testImplementation("io.kotest:kotest-assertions-core:$kotest_version")
  testImplementation("io.kotest:kotest-property:$kotest_version")
}
