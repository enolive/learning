import org.apache.kafka.common.serialization.Serdes
import org.apache.kafka.streams.KafkaStreams
import org.apache.kafka.streams.StreamsBuilder
import org.apache.kafka.streams.StreamsConfig
import java.util.*

fun main(args: Array<String>) {
    val properties = Properties()
    properties[StreamsConfig.APPLICATION_ID_CONFIG] = "streams-fizz-buzz"
    properties[StreamsConfig.BOOTSTRAP_SERVERS_CONFIG] = "localhost:9092"
    properties[StreamsConfig.DEFAULT_KEY_SERDE_CLASS_CONFIG] = Serdes.String().javaClass
    properties[StreamsConfig.DEFAULT_VALUE_SERDE_CLASS_CONFIG] = Serdes.String().javaClass

    val fizzBuzz = FizzBuzz()
    val builder = StreamsBuilder()
    builder.stream<String, String>("streams-fizz-buzz-input")
            .mapValues<Int>(::valueToInt)
            .flatMapValues(fizzBuzz::sequenceUpTo)
            .to("streams-fizz-buzz-output")

    val topology = builder.build()
    val streams = KafkaStreams(topology, properties)

    streams.start()

    Runtime.getRuntime().addShutdownHook(Thread(streams::close))
}

private fun valueToInt(value: String) = value.toInt()