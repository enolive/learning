package de.welcz.springmongo

import de.welcz.springmongo.entity.MessageDoc
import de.welcz.springmongo.control.MessagesRepository
import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.test.context.junit.jupiter.SpringExtension
import java.util.*

@SpringBootTest
@ExtendWith(SpringExtension::class)
class MessagesRepositoryTest {
    @Autowired()
    private lateinit var messagesRepository: MessagesRepository

    @Test
    internal fun `insert works`() {
        val messageDoc = MessageDoc("123", "id", "some text")

        messagesRepository.save(messageDoc)

        assertThat(messagesRepository.findById("123")).isEqualTo(Optional.of(messageDoc))
        assertThat(messagesRepository.findById("does not exist")).isEqualTo(Optional.empty<MessageDoc>())
    }

    @Test
    internal fun `delete works`() {
        messagesRepository.save(MessageDoc("333", "id", "some text"))

        messagesRepository.deleteById("333")

        assertThat(messagesRepository.findById("333")).isEqualTo(Optional.empty<MessageDoc>())
    }
}