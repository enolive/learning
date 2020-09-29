package de.welcz.demo;

import org.bson.types.ObjectId;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/users")
public class UserController {
  private final UserRepository repository;

  public UserController(UserRepository userRepository) {
    this.repository = userRepository;
  }

  @GetMapping("/")
  public Flux<UserEntity> findAll() {
    return repository.findAll();
  }

  @PostMapping("/")
  public Mono<UserEntity> save(@RequestBody UserEntity user) {
    return repository.save(user);
  }

  @DeleteMapping("/{id}")
  public Mono<Void> delete(@PathVariable ObjectId id) {
    return repository.deleteById(id);
  }
}
