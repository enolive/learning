package de.welcz.kafkapublishing;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.support.SendResult;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

@Service
public class Producer {

  private final KafkaTemplate<String, String> stringKafkaTemplate;
  private final KafkaTemplate<String, Shit> shitKafkaTemplate;

  @Autowired
  public Producer(KafkaTemplate<String, String> stringKafkaTemplate,
                  KafkaTemplate<String, Shit> shitKafkaTemplate) {
    this.stringKafkaTemplate = stringKafkaTemplate;
    this.shitKafkaTemplate = shitKafkaTemplate;
  }

  Mono<SendResult<String, String>> publishSimpleMessage(String message) {
    return Mono.fromFuture(stringKafkaTemplate.send(Topics.IMPORT_DATA_AVAILABLE, message).completable());
  }

  Mono<SendResult<String, Shit>> publishShit(Shit shit) {
    return Mono.fromFuture(shitKafkaTemplate.send(Topics.SHIT_HAPPENED, shit).completable());
  }
}
