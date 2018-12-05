package de.welcz.springmongo

import de.flapdoodle.embed.mongo.Command
import de.flapdoodle.embed.mongo.config.DownloadConfigBuilder
import de.flapdoodle.embed.mongo.config.ExtractedArtifactStoreBuilder
import de.flapdoodle.embed.mongo.config.RuntimeConfigBuilder
import de.flapdoodle.embed.process.config.IRuntimeConfig
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

@Configuration
class ConfigureEmbedded(private val configProperties: ConfigProperties) {
    private val command = Command.MongoD

    @Bean
    fun runtimeConfig(): IRuntimeConfig = RuntimeConfigBuilder()
        .defaults(command)
        .artifactStore(ExtractedArtifactStoreBuilder()
            .defaults(command)
            .download(DownloadConfigBuilder()
                .defaultsForCommand(command)
                .downloadPath(configProperties.mongoDownloadPath)
                .build()))
        .build()
}