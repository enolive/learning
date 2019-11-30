package de.welcz.multipartapidocs;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.codec.multipart.FilePart;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.util.UriComponentsBuilder;
import reactor.core.publisher.Mono;

import java.net.URI;
import java.util.UUID;

import static java.text.MessageFormat.format;

@RestController
public class Controller {
  @Operation(description = "uploads a file",
             responses = @ApiResponse(content = @Content(schema = @Schema(implementation = String.class))))
  @PostMapping(value = "/uploads",
               consumes = MediaType.MULTIPART_FORM_DATA_VALUE,
               produces = MediaType.TEXT_PLAIN_VALUE)
  public Mono<ResponseEntity<String>> uploadFile(
      @Parameter(description = "the file to upload")
      @RequestPart(name = "file") FilePart file) {
    return Mono.just(ResponseEntity.created(buildUri("/uploads/{id}", UUID.randomUUID()))
                                   .body(format("uploaded {0} of type {1}",
                                                file.filename(),
                                                file.headers().getContentType()
                                         )
                                   )
    );
  }

  private URI buildUri(String uri, Object id) {
    return UriComponentsBuilder.fromUriString(uri)
                               .build(id);
  }
}
