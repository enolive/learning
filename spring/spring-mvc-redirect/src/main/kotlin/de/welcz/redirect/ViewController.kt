package de.welcz.redirect

import io.github.oshai.kotlinlogging.KotlinLogging
import org.springframework.stereotype.Controller
import org.springframework.ui.Model
import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PostMapping
import org.springframework.web.servlet.mvc.support.RedirectAttributes

@Controller
class ViewController {
  private val log = KotlinLogging.logger {}

  @GetMapping("/")
  fun start(model: Model): String {
    model.addAttribute("slogan", "I hate Spring!!!")
    log.info { "start view" }
    return "index"
  }

  @PostMapping("/calculate")
  fun calculate(
    number1: Int,
    number2: Int,
    redirectAttributes: RedirectAttributes,
  ): String {
    val result = number1 + number2
    log.info { "result: $result" }
    redirectAttributes.addFlashAttribute("result", result)
    // this demonstrates a different behavior when the JSESSIONID cookie is not set,
    // the code set in the index() method won't be called!
    // however, by changing the tracking-modes in application.yaml, this strange behavior goes away
    // additionally there are some other different behaviors:
    // - when the template for / is not called `index`, we will get a 404 page
    // - the URL will contain the JSESSIONID cookie in the form <URL>;JSESSIONID=...
    // - when we redirect to something else than /, none of these apply
    redirectAttributes.addFlashAttribute(
      "slogan",
      "We don't need no water let the motherfucker burn, burn motherfucker!"
    )
    return "redirect:/"
  }
}