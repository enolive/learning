package de.welcz.kafkapublishing;

import lombok.extern.slf4j.Slf4j;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class Consumer {
  @KafkaListener(topics = Topics.IMPORT_DATA_AVAILABLE,
                 containerFactory = "kafkaListenerContainerFactory")
  public void listen(String message) {
    LOGGER.info("received message {}", message);
  }

  @KafkaListener(topics = Topics.SHIT_HAPPENED,
                 containerFactory = "shitContainerFactory")
  public void listen(Shit shit) {
    LOGGER.info("some serious shit happened: {}", shit);
  }
}
