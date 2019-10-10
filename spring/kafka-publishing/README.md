# Spring Kafka with Confluent Platform

I used the [confluent docker quickstart](https://docs.confluent.io/current/quickstart/ce-quickstart.html) 
for an easy setup of kafka.

This sample creates a simple reactive rest controller
and a publisher/consumer for two kafka topics.

It wasn't easy to get automatic JSON parsing running.
I had to inject the Jackson Object Mapper into a new JsonDeserializer
and also specify the entity package name as a trusted package.
Otherwise, either the deserialization of things 
like date values (JSR310) fails or the entity package isn't trusted.
