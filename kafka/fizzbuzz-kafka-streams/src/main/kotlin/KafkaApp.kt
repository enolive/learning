import org.apache.kafka.common.serialization.Serde
import org.apache.kafka.common.serialization.Serdes
import org.apache.kafka.streams.KafkaStreams
import org.apache.kafka.streams.StreamsBuilder
import org.apache.kafka.streams.Topology
import org.apache.kafka.streams.kstream.Consumed
import org.apache.kafka.streams.kstream.Produced
import java.util.*

object KafkaApp {
    const val INPUT_TOPIC = "streams-fizz-buzz-input"

    const val OUTPUT_TOPIC = "streams-fizz-buzz-output"

    val keySerdes: Serde<String> = Serdes.String()

    val valueSerdes: Serde<String> = Serdes.String()

    @JvmStatic
    fun main(args: Array<String>) {
        val fizzBuzz = FizzBuzzService()
        val streams = KafkaStreams(getTopology(fizzBuzz), getConfig())
        streams.start()

        Runtime.getRuntime().addShutdownHook(Thread(streams::close))
    }

    fun getTopology(fizzBuzzService: FizzBuzzService): Topology {
        val builder = StreamsBuilder()
        builder.stream<String, String>(INPUT_TOPIC, Consumed.with(keySerdes, valueSerdes))
                .mapValues<Int>(::valueToInt)
                .filter { _, limit -> limit < 10000 }
                .flatMapValues(fizzBuzzService::sequenceUpTo)
                .to(OUTPUT_TOPIC, Produced.with(keySerdes, valueSerdes))
        return builder.build()
    }

    private fun valueToInt(value: String) = value.toIntOrNull() ?: 0

    private fun getConfig(): Properties {
        val config = Properties()
        config.load(javaClass.getResourceAsStream("kafka.properties"))
        return config
    }
}