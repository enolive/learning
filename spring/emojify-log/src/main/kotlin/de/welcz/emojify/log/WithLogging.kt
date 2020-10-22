package de.welcz.emojify.log

import org.slf4j.Logger
import org.slf4j.LoggerFactory

abstract class WithLogging {
  val logger: Logger = getLoggerFor()

  private inline fun <reified T> T.getLoggerFor() = when {
    T::class.isCompanion -> LoggerFactory.getLogger(T::class.java.enclosingClass)
    else                 -> LoggerFactory.getLogger(T::class.java)
  }
}

