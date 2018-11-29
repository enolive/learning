package de.welcz.springmongo

import com.nhaarman.mockito_kotlin.*
import de.welcz.springmongo.entity.MessageDoc
import de.welcz.springmongo.control.MessagesRepository
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest
import org.springframework.boot.test.mock.mockito.MockBean
import org.springframework.http.MediaType
import org.springframework.test.context.junit.jupiter.SpringExtension
import org.springframework.test.web.servlet.MockMvc
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*
import org.springframework.test.web.servlet.result.MockMvcResultMatchers.content
import org.springframework.test.web.servlet.result.MockMvcResultMatchers.status
import java.util.*

@WebMvcTest
@ExtendWith(SpringExtension::class)
class MessagesControllerTest(@Autowired val mockMvc: MockMvc) {
    @MockBean
    private lateinit var repository: MessagesRepository

    @Test
    internal fun `loads data`() {
        whenever(repository.findById(any())).thenReturn(Optional.of(MessageDoc("123", "my id", "my text")))
        mockMvc.perform(get("/messages/client/12345/consultant/67890").accept(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk)
            .andExpect(content()
                .json("""{"client": 12345, "consultant": 67890, "messageId": "my id", "messageText": "my text"}"""))
        verify(repository, times(1)).findById("12345-67890")
    }

    @Test
    internal fun `deletes data`() {
        mockMvc.perform(delete("/messages/client/12345/consultant/67890"))
            .andExpect(status().isNoContent)
        verify(repository, times(1)).deleteById("12345-67890")
    }

    @Test
    internal fun `saves data`() {
        mockMvc.perform(
                put("/messages")
                    .contentType(MediaType.APPLICATION_JSON)
                    .content("""{"client": 123, "consultant": 555, "messageId": "my id", "messageText": "my text"}"""))
            .andExpect(status().isNoContent)
        verify(repository, times(1)).save(eq(MessageDoc("123-555", "my id", "my text")))
    }
}