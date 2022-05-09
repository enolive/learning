import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
  kotlin("jvm") version "1.6.20"
}

group = "org.example"
version = "1.0-SNAPSHOT"

repositories {
  mavenCentral()
}

val kotestVersion: String by project
val jqwikVersion: String by project

dependencies {
  testImplementation("io.kotest:kotest-runner-junit5:$kotestVersion")
  testImplementation("io.kotest:kotest-assertions-core:$kotestVersion")
  testImplementation("io.kotest:kotest-property:$kotestVersion")
  testImplementation("io.kotest:kotest-framework-datatest:$kotestVersion")
  testImplementation("net.jqwik:jqwik:$jqwikVersion")
  testImplementation("net.jqwik:jqwik-kotlin:$jqwikVersion")
}

tasks.test {
  useJUnitPlatform()
}

tasks.withType<KotlinCompile> {
  kotlinOptions {
    jvmTarget = "11"
    freeCompilerArgs = listOf(
      "-Xjsr305=strict", // Required for strict interpretation of
      "-Xemit-jvm-type-annotations" // Required for annotations on type variables
    )
    javaParameters = true // Required to get correct parameter names in reporting
  }
}
