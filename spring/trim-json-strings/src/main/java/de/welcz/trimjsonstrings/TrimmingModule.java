package de.welcz.trimjsonstrings;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.module.SimpleModule;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

import java.io.IOException;

@Component
public class TrimmingModule {
  @Bean
  public SimpleModule addTrimmingSerializer() {
    return new SimpleModule().addDeserializer(String.class, new TrimmingDeserializer());
  }

  private static class TrimmingDeserializer extends JsonDeserializer<String> {
    @Override
    public String deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
      return p.getValueAsString().trim();
    }
  }
}
