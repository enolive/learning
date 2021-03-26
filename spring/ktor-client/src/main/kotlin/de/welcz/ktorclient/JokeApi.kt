package de.welcz.ktorclient

import io.ktor.client.*
import io.ktor.client.call.*
import io.ktor.client.request.*
import io.ktor.client.statement.*
import io.ktor.utils.io.*
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.flow
import org.springframework.stereotype.Service

@Service
class JokeApi(private val httpClient: HttpClient) {
  suspend fun fetchRandomJoke(): JokeResponse = httpClient.get("https://api.chucknorris.io/jokes/random")
}
