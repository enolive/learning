package kafka.streams.demo;

import org.apache.kafka.common.serialization.Serdes;
import org.apache.kafka.streams.KafkaStreams;
import org.apache.kafka.streams.StreamsBuilder;
import org.apache.kafka.streams.StreamsConfig;
import org.apache.kafka.streams.kstream.KStream;

import java.util.Arrays;
import java.util.Properties;
import java.util.concurrent.CountDownLatch;

public class LineSplit {
    public static void main(String[] args) {
        final var properties = new Properties();
        properties.put(StreamsConfig.APPLICATION_ID_CONFIG, "streams-linesplit");
        properties.put(StreamsConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092");
        properties.put(StreamsConfig.DEFAULT_KEY_SERDE_CLASS_CONFIG, Serdes.String().getClass());
        properties.put(StreamsConfig.DEFAULT_VALUE_SERDE_CLASS_CONFIG, Serdes.String().getClass());

        final var builder = new StreamsBuilder();
        final var source = builder.<String, String>stream("streams-plaintext-input");
        source.flatMapValues(value -> Arrays.asList(value.split("\\W+")))
              .to("streams-linesplit-output");

        final var topology = builder.build();
        final var streams = new KafkaStreams(topology, properties);
        final var latch = new CountDownLatch(1);

        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            streams.close();
            latch.countDown();
        }));

        try {
            streams.start();
            latch.await();
        } catch (Throwable e) {
            System.exit(1);
        }
        System.exit(0);
    }
}
