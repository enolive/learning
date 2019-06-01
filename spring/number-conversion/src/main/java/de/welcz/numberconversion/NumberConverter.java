package de.welcz.numberconversion;

import io.vavr.CheckedFunction1;
import io.vavr.control.Option;
import io.vavr.control.Try;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;

@Service
public class NumberConverter {

  private final DecimalFormat currencyFormat = new DecimalFormat("0,00") {{
    setParseBigDecimal(true);
  }};

  public BigDecimal toBigDecimal(String input) {
    return Option.of(input)
                 .filter(i -> !i.isEmpty())
                 .map(CheckedFunction1.liftTry(currencyFormat::parse))
                 .flatMap(Try::toOption)
                 .map(number -> (BigDecimal) number)
                 .map(bigDecimal -> bigDecimal.setScale(2, RoundingMode.HALF_UP))
                 .getOrElse((BigDecimal) null);
  }
}
