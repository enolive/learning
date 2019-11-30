# Camunda with WebFlux

working complete example of a camunda process hosted
inside a spring boot reactive web app.

## What works

* All kinds of Fluxy REST endpoints ;-)
* Communication with camunda
* Java Delegates

## What doesn't

* The cockpit needs a servlet container. 
  So it is not included here. You can always
  make a separate app based on 
  `camunda-spring-boot-starter-webapp` accessing the same db. 
  By that way, you gain more
  control who can actually enter your cockpit and
  can introduce a different scalability plan to it.
* Accessing the process variables inside a `JavaDelegate` inside
  WebClient's reactive response thread will produce race conditions
  or precondition failures (depending on the execution model
  and whether you use `RunTimeService` or the `DelegateExecution`).
  See also 
  [my question in  the camunda forum](https://forum.camunda.org/t/unable-to-read-write-process-variables-in-webfluxs-webclient-reactive-chain/16283/5).
* Non-Blocking calls inside a `JavaDelegate` are not possible
  due to its API restriction (void execute method).
