package de.welcz.demo;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.data.mongo.DataMongoTest;

import static org.assertj.core.api.Assertions.assertThat;

@DataMongoTest
public class UserRepositoryTest {
  @Autowired
  private UserRepository userRepository;

  @Test
  void new_data_is_saved() {
    var user = new UserEntity("Chris", 42);

    var savedUser = userRepository.save(user).block();

    var storedUser = userRepository.findById(savedUser.getId()).block();
    assertThat(storedUser).isEqualTo(user);
  }

  @Test
  void existing_data_is_modified() {
    var initiallySavedUser = userRepository.save(new UserEntity("Chris", 42)).block();
    var modifiedUser = new UserEntity(initiallySavedUser.getId(), "Updated", -1);

    userRepository.save(modifiedUser).block();

    var storedUser = userRepository.findById(initiallySavedUser.getId()).block();
    assertThat(storedUser).isEqualTo(modifiedUser)
                          .isNotEqualTo(initiallySavedUser);
  }
}
