import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
  kotlin("jvm") version "1.6.20"
}

group = "org.example"
version = "1.0-SNAPSHOT"

repositories {
  mavenCentral()
}

dependencies {
  testImplementation(kotlin("test"))
  testImplementation("net.jqwik:jqwik-kotlin:1.6.5")
  testImplementation("io.kotest:kotest-assertions-core:5.3.0")
}

tasks.test {
  useJUnitPlatform()
}

tasks.withType<KotlinCompile> {
  kotlinOptions {
    freeCompilerArgs = listOf(
      "-Xjsr305=strict", // Required for strict interpretation of
      "-Xemit-jvm-type-annotations" // Required for annotations on type variables
    )
    jvmTarget = "11" // 1.8 or above
    javaParameters = true // Required to get correct parameter names in reporting
  }
}
