val ktor_version: String by project
val kotlin_version: String by project
val logback_version: String by project
val kotest_version: String by project

plugins {
  application
  kotlin("jvm") version "1.4.32"
  kotlin("plugin.serialization") version "1.4.32"
}

group = "com.example"
version = "0.0.1"
application {
  mainClass.set("io.ktor.server.netty.EngineMain")
}

repositories {
  mavenCentral()
}

dependencies {
  implementation("io.ktor:ktor-server-core:$ktor_version")
  implementation("io.ktor:ktor-server-netty:$ktor_version")
  implementation("ch.qos.logback:logback-classic:$logback_version")
  implementation("io.ktor:ktor-serialization:$ktor_version")
  testImplementation("io.ktor:ktor-server-tests:$ktor_version")
  testImplementation("io.kotest:kotest-runner-junit5:$kotest_version")
  testImplementation("io.kotest:kotest-assertions-core:$kotest_version")
  testImplementation("io.kotest:kotest-assertions-ktor:$kotest_version")
  testImplementation("io.kotest:kotest-assertions-json:$kotest_version")
  testImplementation("io.kotest:kotest-property:$kotest_version")
}

tasks.withType<Test> {
  useJUnitPlatform()
}
