# Spring Data R2DBC

Just a show case how to use a relational database inside a reactive spring stack.

## What works

* Object Mapping via Reactive CrudRepositories
* CRUD Operations via REST

## Limitations

* By design no real ORM, see also [docs](https://github.com/spring-projects/spring-data-r2dbc#this-is-not-an-orm)
    * more comparable to Spring Data JDBC than to JPA
    * No Auto-DDL capabilities like in JPA, use `liquibase` instead
* liquibase does not support r2dbc yet, so you additionally need to include jdbc
* No built-in Bean Validation support
    * you have to add `spring-boot-starter-validation` and annotate your Controller methods with `@Valid`
  