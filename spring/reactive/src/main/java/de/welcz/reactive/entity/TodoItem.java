package de.welcz.reactive.entity;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.databind.ser.std.ToStringSerializer;
import lombok.AllArgsConstructor;
import lombok.Value;
import org.bson.types.ObjectId;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Value
@Document(collection = "todos")
@AllArgsConstructor
public class TodoItem {
  @Id
  @JsonSerialize(using = ToStringSerializer.class)
  ObjectId id;
  String title;
  TodoItemPriority priority;
  boolean done;

  public static TodoItem from(InsertTodoItem item) {
    return new TodoItem(ObjectId.get(),
                        item.getTitle(),
                        item.getPriority(),
                        false);
  }
}
