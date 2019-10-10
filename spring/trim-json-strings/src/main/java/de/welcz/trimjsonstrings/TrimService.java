package de.welcz.trimjsonstrings;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

@Service
public class TrimService {
  private final ObjectMapper objectMapper;

  @Autowired
  public TrimService(ObjectMapper objectMapper) {
    this.objectMapper = objectMapper;
  }

  public String trim(String input) throws IOException {
    final TypeReference<HashMap<String, Object>> reference =
        new TypeReference<HashMap<String, Object>>() {
        };
    Map<String, Object> map = objectMapper.readValue(input, reference);
    return objectMapper.writeValueAsString(map);
  }
}
