# Simple RabbitMQ showcase

Simple app that uses RabbitMQ in the context of Spring Cloud Stream.

It defines two processors (uppercase and reverse) and a simple consumer for the uppercase out message.

I used the [rabbitmq:3:management docker image](https://codeburst.io/get-started-with-rabbitmq-on-docker-4428d7f6e46b) for a local RabbitMQ.

```bash
docker run --rm -it --hostname my-rabbit -p 15672:15672 -p 5672:5672 rabbitmq:3-management --name my-rabbit
```

I also used the [quickstart to Spring Cloud Stream](https://docs.spring.io/spring-cloud-stream/docs/3.2.1/reference/html/spring-cloud-stream.html#_quick_start) for my first experiments but adapted all code to Kotlin.
