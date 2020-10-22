# Logback with emojis

Simple spring boot project that uses color emojis in its log.

The magic happens in `src/main/resources/logback-spring.xml`.

There is also a custom converter `EmojiLevelConverter` that implements log level emojis
that is optional.

There are two profiles in the application:

- default: simple emoji support
- level-emojis: display a different emoji depending on the log level