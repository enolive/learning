package de.welcz.numberconversion;

import io.vavr.jackson.datatype.VavrModule;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class CustomizeJackson {
  @Bean
  public VavrModule registerVavrModule() {
    return new VavrModule();
  }
}
