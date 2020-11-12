package de.welcz.cochucknorris

import org.springframework.context.annotation.Configuration

@Configuration
data class FetchConfig(val host: String = "https://api.chucknorris.io/")
