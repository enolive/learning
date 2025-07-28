# Spring Boot Redirect Demo

A Spring Boot application demonstrating redirect functionality with flash attributes and session tracking using Kotlin.

It also demonstrates the session handling in Spring Boot
that behaves differently depending on whether the `JSESSIONID` cookie is already set. This happens only when the redirect page
is the main index page and when tracking-modes is not
explicitly set to `cookie`.

## Prerequisites

- JDK 21 or higher
- Kotlin 1.9+

## Running the Application

1. Clone the repository
2. Navigate to the project directory
3. Run `./gradlew bootRun` (Gradle)
4. Open http://localhost:8080 in your browser
