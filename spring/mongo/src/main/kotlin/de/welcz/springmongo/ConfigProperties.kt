package de.welcz.springmongo

import org.springframework.boot.context.properties.ConfigurationProperties

@ConfigurationProperties(prefix = "custom")
class ConfigProperties {
    var mongoDownloadPath: String = "http://fastdl.mongodb.org/"
}