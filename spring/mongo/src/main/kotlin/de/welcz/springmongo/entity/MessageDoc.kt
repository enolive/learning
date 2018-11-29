package de.welcz.springmongo.entity

import org.springframework.data.annotation.Id
import org.springframework.data.mongodb.core.mapping.Document

@Document(collection = "messages")
data class MessageDoc(@Id val id: String, val messageId: String, val messageText: String)
