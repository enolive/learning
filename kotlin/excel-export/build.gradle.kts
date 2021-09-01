plugins {
  kotlin("jvm") version "1.5.10"
}

group = "org.example"
version = "1.0-SNAPSHOT"

val kotest_version: String by project
val poi_version: String by project

repositories {
  mavenCentral()
}

tasks.getByName<Test>("test") {
  useJUnitPlatform()
}

dependencies {
  implementation(kotlin("stdlib"))
  implementation("org.apache.poi:poi:$poi_version")
  implementation("org.apache.poi:poi-ooxml:$poi_version")
  testImplementation("io.kotest:kotest-runner-junit5:$kotest_version")
  testImplementation("io.kotest:kotest-assertions-core:$kotest_version")
}
