package de.welcz.springmongo.boundary

data class MessageDto(val client: Long, val consultant: Long, val messageId: String, val messageText: String)
