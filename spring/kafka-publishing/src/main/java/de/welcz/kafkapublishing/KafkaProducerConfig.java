package de.welcz.kafkapublishing;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.kafka.common.serialization.StringSerializer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.kafka.KafkaProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.core.DefaultKafkaProducerFactory;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.core.ProducerFactory;
import org.springframework.kafka.support.serializer.JsonSerializer;

import java.util.HashMap;
import java.util.Map;

@Configuration
public class KafkaProducerConfig {
  private final KafkaProperties kafkaProperties;
  private final ObjectMapper objectMapper;

  @Autowired
  public KafkaProducerConfig(KafkaProperties kafkaProperties, ObjectMapper objectMapper) {
    this.kafkaProperties = kafkaProperties;
    this.objectMapper = objectMapper;
  }

  @Bean
  public ProducerFactory<String, String> stringProducerFactory() {
    final Map<String, Object> config = kafkaProperties.buildProducerProperties();
    return new DefaultKafkaProducerFactory<>(config);
  }

  @Bean
  public KafkaTemplate<String, String> stringKafkaTemplate() {
    return new KafkaTemplate<>(stringProducerFactory());
  }

  @Bean
  public ProducerFactory<String, Shit> shitProducerFactory() {
    final Map<String, Object> config = kafkaProperties.buildProducerProperties();
    return new DefaultKafkaProducerFactory<>(config,
                                             new StringSerializer(),
                                             new JsonSerializer<>(objectMapper));
  }

  @Bean
  public KafkaTemplate<String, Shit> shitKafkaTemplate() {
    return new KafkaTemplate<>(shitProducerFactory());
  }
}
