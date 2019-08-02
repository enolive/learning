package de.welcz.reactive.boundary;

import de.welcz.reactive.control.TodoRepository;
import de.welcz.reactive.entity.InsertTodoItem;
import de.welcz.reactive.entity.TodoItem;
import de.welcz.reactive.entity.TodoItemPriority;
import org.bson.types.ObjectId;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SynchronousSink;

import java.security.SecureRandom;

@RestController
public class ReactiveController {
  private final TodoRepository repository;

  @Autowired
  public ReactiveController(TodoRepository repository) {
    this.repository = repository;
  }

  @PutMapping("/todos")
  public Mono<TodoItem> update(@RequestBody TodoItem item) {
    return repository.save(item);
  }

  @PostMapping("/todos")
  public Mono<TodoItem> createTodo(@RequestBody Mono<InsertTodoItem> item) {
    return item.map(TodoItem::from)
               .flatMap(repository::save);
  }

  @GetMapping("/todos")
  public Flux<TodoItem> findAll() {
    return repository.findAll();
  }

  @DeleteMapping("/todos")
  public Mono<Void> deleteAll() {
    return repository.deleteAll();
  }

  @GetMapping("/todos/title/{title}")
  public Flux<TodoItem> findByTitle(@PathVariable String title) {
    return repository.findAllByTitle(title);
  }

  @GetMapping(value = "/todos/generated")
  public Flux<TodoItem> getSampleData() {
    return Flux.generate(() -> 1, this::nextNumber)
               .map(integer -> new InsertTodoItem("Item no " + integer, randomPriority()))
               .map(TodoItem::from);
  }

  private Integer nextNumber(Integer current, SynchronousSink<Integer> synchronousSink) {
    synchronousSink.next(current);
    return current + 1;
  }

  private TodoItemPriority randomPriority() {
    final var ordinal = new SecureRandom().nextInt(TodoItemPriority.values().length);
    return TodoItemPriority.values()[ordinal];
  }

  @DeleteMapping("/todos/{id}")
  public Mono<Void> deleteById(@PathVariable ObjectId id) {
    return repository.deleteById(id);
  }
}
