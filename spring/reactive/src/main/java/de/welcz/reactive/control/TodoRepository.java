package de.welcz.reactive.control;

import de.welcz.reactive.entity.TodoItem;
import org.bson.types.ObjectId;
import org.springframework.data.mongodb.repository.ReactiveMongoRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;

@Repository
public interface TodoRepository extends ReactiveMongoRepository<TodoItem, ObjectId> {
  Flux<TodoItem> findAllByTitle(String title);
}
