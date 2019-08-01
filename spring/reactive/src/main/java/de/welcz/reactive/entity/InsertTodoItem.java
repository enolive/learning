package de.welcz.reactive.entity;

import lombok.Value;

@Value
public class InsertTodoItem {
  String title;
  TodoItemPriority priority;
}
