package de.welcz.springmongo

import de.welcz.springmongo.entity.MessageDoc
import de.welcz.springmongo.repository.MessagesRepository
import org.slf4j.LoggerFactory
import org.springframework.boot.CommandLineRunner
import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.context.annotation.Bean

@SpringBootApplication
class Application {
    private val log = LoggerFactory.getLogger(Application::class.java)

    @Bean
    fun run(repository: MessagesRepository) = CommandLineRunner {
        repository.save(MessageDoc("12345-67890", "#12345", "Test"))
        repository.save(MessageDoc("2", "#34423", "Another one"))
        repository.save(MessageDoc("3", "#88877", "Yet another one"))
        log.info("data created!")

        repository.findById("1").ifPresent { found -> log.info("found $found") }
        log.info("okay, finished!")
    }
}

fun main(args: Array<String>) {
    runApplication<Application>(*args)
}
