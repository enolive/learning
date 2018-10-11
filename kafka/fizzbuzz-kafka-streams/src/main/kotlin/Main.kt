import org.apache.kafka.common.serialization.Serdes
import org.apache.kafka.streams.KafkaStreams
import org.apache.kafka.streams.StreamsBuilder
import org.apache.kafka.streams.StreamsConfig

fun main(args: Array<String>) {
    val fizzBuzz = FizzBuzz()
    val builder = StreamsBuilder()
    builder.stream<String, String>("streams-fizz-buzz-input")
            .mapValues<Int>(::valueToInt)
            .flatMapValues(fizzBuzz::sequenceUpTo)
            .to("streams-fizz-buzz-output")

    val topology = builder.build()
    val streams = KafkaStreams(topology, getKafkaProperties())

    streams.start()

    Runtime.getRuntime().addShutdownHook(Thread(streams::close))
}

private fun getKafkaProperties() = mapOf(
        StreamsConfig.APPLICATION_ID_CONFIG to "streams-fizz-buzz",
        StreamsConfig.BOOTSTRAP_SERVERS_CONFIG to "localhost:9092",
        StreamsConfig.DEFAULT_KEY_SERDE_CLASS_CONFIG to Serdes.String().javaClass.name,
        StreamsConfig.DEFAULT_VALUE_SERDE_CLASS_CONFIG to Serdes.String().javaClass.name
).toProperties()

private fun valueToInt(value: String) = value.toIntOrNull() ?: 0