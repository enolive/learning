package de.welcz.reactive;

import de.welcz.reactive.control.TodoRepository;
import de.welcz.reactive.entity.InsertTodoItem;
import de.welcz.reactive.entity.TodoItem;
import de.welcz.reactive.entity.TodoItemPriority;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import reactor.core.publisher.Flux;

@SpringBootApplication
public class ReactiveApplication {
  private final TodoRepository repository;

  public ReactiveApplication(TodoRepository repository) {
    this.repository = repository;
  }

  public static void main(String[] args) {
    SpringApplication.run(ReactiveApplication.class, args);
  }

  @Bean
  CommandLineRunner run() {
    return args -> repository.deleteAll()
                             .thenMany(todoItems())
                             .log()
                             .map(TodoItem::from)
                             .flatMap(repository::save)
                             .subscribe();
  }

  private Flux<InsertTodoItem> todoItems() {
    return Flux.just(new InsertTodoItem("Learn Haskell!", TodoItemPriority.LOW),
                     new InsertTodoItem("Just do it", TodoItemPriority.MEDIUM),
                     new InsertTodoItem("Switch to a better language 😉", TodoItemPriority.LOW),
                     new InsertTodoItem("Be nice", TodoItemPriority.HIGH),
                     new InsertTodoItem("Take care of yourself", TodoItemPriority.HIGH)
    );
  }

}
