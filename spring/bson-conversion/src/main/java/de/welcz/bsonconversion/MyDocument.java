package de.welcz.bsonconversion;

import lombok.Data;
import org.bson.types.ObjectId;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.time.YearMonth;
import java.time.ZonedDateTime;

@Data
@Document
public class MyDocument {
  @Id
  ObjectId id;
  final ZonedDateTime dateTime;
  final YearMonth yearMonth;
}
