package de.welcz.kafkapublishing;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.kafka.KafkaProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.config.ConcurrentKafkaListenerContainerFactory;
import org.springframework.kafka.core.ConsumerFactory;
import org.springframework.kafka.core.DefaultKafkaConsumerFactory;
import org.springframework.kafka.support.serializer.JsonDeserializer;

import java.util.Map;

@Configuration
public class KafkaConsumerConfig {
  private final KafkaProperties kafkaProperties;
  private final ObjectMapper objectMapper;

  @Autowired
  public KafkaConsumerConfig(KafkaProperties kafkaProperties, ObjectMapper objectMapper) {
    this.kafkaProperties = kafkaProperties;
    this.objectMapper = objectMapper;
  }

  @Bean
  public ConsumerFactory<String, String> consumerFactory() {
    final Map<String, Object> config = kafkaProperties.buildConsumerProperties();
    return new DefaultKafkaConsumerFactory<>(config);
  }

  @Bean
  public ConcurrentKafkaListenerContainerFactory<String, String> kafkaListenerContainerFactory() {
    final ConcurrentKafkaListenerContainerFactory<String, String> factory =
        new ConcurrentKafkaListenerContainerFactory<>();
    factory.setConsumerFactory(consumerFactory());
    return factory;
  }

  @Bean
  public ConsumerFactory<String, Shit> shitConsumerFactory() {
    final Map<String, Object> config = kafkaProperties.buildConsumerProperties();
    final JsonDeserializer<Shit> deserializer = new JsonDeserializer<>(objectMapper);
    deserializer.addTrustedPackages(Shit.class.getPackage().getName());
    return new DefaultKafkaConsumerFactory<>(config,
                                             new StringDeserializer(),
                                             deserializer);
  }

  @Bean
  public ConcurrentKafkaListenerContainerFactory<String, Shit> shitContainerFactory() {
    final ConcurrentKafkaListenerContainerFactory<String, Shit> factory =
        new ConcurrentKafkaListenerContainerFactory<>();
    factory.setConsumerFactory(shitConsumerFactory());
    return factory;
  }
}
