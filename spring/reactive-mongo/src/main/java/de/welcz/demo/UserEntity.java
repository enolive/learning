package de.welcz.demo;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.databind.ser.std.ToStringSerializer;
import lombok.*;
import org.bson.types.ObjectId;
import org.springframework.data.annotation.Id;

@Data
@AllArgsConstructor
@NoArgsConstructor
@RequiredArgsConstructor
public class UserEntity {
  @Id
  @JsonSerialize(using = ToStringSerializer.class)
  private ObjectId id;
  @NonNull
  private String name;
  @NonNull
  private int age;
}
