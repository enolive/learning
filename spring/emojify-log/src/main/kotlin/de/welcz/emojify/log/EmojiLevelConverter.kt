package de.welcz.emojify.log

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.pattern.ClassicConverter
import ch.qos.logback.classic.spi.ILoggingEvent

class EmojiLevelConverter : ClassicConverter() {
  // be careful using emojis with different char length here because it might break the log padding
  override fun convert(event: ILoggingEvent): String = when (event.level) {
    Level.INFO  -> "\uD83D\uDCA1"
    Level.WARN  -> "\uD83D\uDEA7"
    Level.ERROR -> "\uD83D\uDED1"
    Level.TRACE -> "\uD83D\uDD0E"
    Level.DEBUG -> "\uD83D\uDC1B"
    else        -> " "
  }
}