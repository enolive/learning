package de.welcz.springdatarestwithpostgres;

import org.springframework.context.annotation.Configuration;
import org.springframework.data.rest.core.annotation.HandleBeforeCreate;
import org.springframework.data.rest.core.annotation.HandleBeforeSave;
import org.springframework.data.rest.core.annotation.RepositoryEventHandler;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

@Configuration
@RepositoryEventHandler
public class ProductEventHandler {
  @HandleBeforeSave
  @HandleBeforeCreate
  public void onSave(Product product) {
    if (product.getName().equals("Fibu")) {
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Don't say such nasty words");
    }
  }
}
