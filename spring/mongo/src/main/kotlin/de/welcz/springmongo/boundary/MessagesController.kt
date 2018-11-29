package de.welcz.springmongo.boundary

import de.welcz.springmongo.repository.MessagesRepository
import de.welcz.springmongo.entity.MessageDoc
import org.springframework.http.HttpStatus
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/messages")
class MessagesController(val repository: MessagesRepository) {
    @GetMapping("/client/{client}/consultant/{consultant}")
    fun findMessage(@PathVariable("client") client: Long,
                    @PathVariable("consultant") consultant: Long): MessageDto? =
            repository
                .findById(toId(client, consultant))
                .map { MessageDto(client, consultant, it.messageId, it.messageText) }
                .orElse(null)

    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping
    fun saveMessage(@RequestBody message: MessageDto) {
        repository.save(MessageDoc(toId(message), message.messageId, message.messageText))
    }

    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("/client/{client}/consultant/{consultant}")
    fun deleteMessage(@PathVariable("client") client: Long,
                      @PathVariable("consultant") consultant: Long) {
        repository.deleteById(toId(client, consultant))
    }

    private fun toId(message: MessageDto) = toId(message.client, message.consultant)

    private fun toId(client: Long, consultant: Long) = "$client-$consultant"
}