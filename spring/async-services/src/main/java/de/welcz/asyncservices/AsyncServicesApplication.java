package de.welcz.asyncservices;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableAsync;

@SpringBootApplication
@EnableAsync
public class AsyncServicesApplication {

    public static void main(String[] args) {
        SpringApplication.run(AsyncServicesApplication.class, args);
    }

}
