package de.welcz.springmongo

import de.welcz.springmongo.entity.MessageDoc
import de.welcz.springmongo.control.MessagesRepository
import org.slf4j.LoggerFactory
import org.springframework.boot.CommandLineRunner
import org.springframework.boot.autoconfigure.EnableAutoConfiguration
import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.autoconfigure.mongo.embedded.EmbeddedMongoAutoConfiguration
import org.springframework.boot.runApplication
import org.springframework.context.annotation.Bean

@SpringBootApplication(exclude = [EmbeddedMongoAutoConfiguration::class])
class Application {
    private val log = LoggerFactory.getLogger(Application::class.java)

    @Bean
    fun run(repository: MessagesRepository) = CommandLineRunner {
        repository.findById("12345-67890").ifPresent { found -> log.info("found $found") }

        repository.save(MessageDoc("12345-67890", "#12345", "Test"))
        repository.save(MessageDoc("2222-3333", "#34423", "Another one"))
        repository.save(MessageDoc("456-789", "#88877", "Yet another one"))
        log.info("data created!")

        repository.findById("12345-67890").ifPresent { found -> log.info("found $found") }
        log.info("okay, finished!")
    }
}

fun main(args: Array<String>) {
    runApplication<Application>(*args)
}
