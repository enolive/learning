import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.streams.StreamsConfig
import org.apache.kafka.streams.TopologyTestDriver
import org.apache.kafka.streams.test.ConsumerRecordFactory
import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import org.mockito.Mock
import org.mockito.Mockito.*
import org.mockito.junit.jupiter.MockitoExtension
import java.util.*


@ExtendWith(MockitoExtension::class)
class KafkaAppTest {

    private lateinit var testDriver: TopologyTestDriver

    private lateinit var factory: ConsumerRecordFactory<String, String>

    @Mock
    private lateinit var fizzBuzzService: FizzBuzzService

    @BeforeEach
    internal fun setUp() {
        val topology = KafkaApp.getTopology(fizzBuzzService)
        val config = getDummyConfig()
        testDriver = TopologyTestDriver(topology, config)
        factory = ConsumerRecordFactory(
                KafkaApp.INPUT_TOPIC,
                KafkaApp.keySerdes.serializer(),
                KafkaApp.valueSerdes.serializer())
    }

    @AfterEach
    internal fun tearDown() {
        testDriver.close()
    }

    @Test
    internal fun `reads fizz buzz output`() {
        `when`(fizzBuzzService.sequenceUpTo(anyInt())).thenReturn(listOf("This", "is", "my", "output"))

        testDriver.pipeInput(factory.create("15"))

        val seq = readAllFromOutput().toList()
        assertThat(seq).containsExactly("This", "is", "my", "output")
        verify(fizzBuzzService, times(1)).sequenceUpTo(15)
    }

    @Test
    internal fun `rejects input over upper limit`() {
        testDriver.pipeInput(factory.create("10000"))

        assertThat(readFromOutput()).isNull()
        verify(fizzBuzzService, times(0)).sequenceUpTo(anyInt())
    }

    private fun readAllFromOutput(): Sequence<String> =
            generateSequence(this::readFromOutput)
                    .map { record -> record.value() }

    private fun readFromOutput(): ProducerRecord<String, String>? = testDriver.readOutput(
            KafkaApp.OUTPUT_TOPIC,
            KafkaApp.keySerdes.deserializer(),
            KafkaApp.valueSerdes.deserializer())

    private fun getDummyConfig(): Properties = mapOf(
            StreamsConfig.APPLICATION_ID_CONFIG to "test",
            StreamsConfig.BOOTSTRAP_SERVERS_CONFIG to "dummy:1234"
    ).toProperties()
}