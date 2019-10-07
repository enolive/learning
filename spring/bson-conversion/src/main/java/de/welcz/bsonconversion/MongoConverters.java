package de.welcz.bsonconversion;

import org.bson.Document;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.convert.converter.Converter;
import org.springframework.data.convert.ReadingConverter;
import org.springframework.data.convert.WritingConverter;
import org.springframework.data.mongodb.core.convert.MongoCustomConversions;

import java.time.*;
import java.util.Arrays;

@Configuration
public class MongoConverters {
  @Bean
  public MongoCustomConversions conversions() {
    return new MongoCustomConversions(Arrays.asList(new ReadYearMonthConverter(),
                                                    new WriteYearMonthConverter(),
                                                    new ReadZonedDateTimeConverter(),
                                                    new WriteZonedDateTimeConverter()
    ));
  }

  @ReadingConverter
  static class ReadYearMonthConverter implements Converter<Document, YearMonth> {
    @Override
    public YearMonth convert(Document document) {
      return YearMonth.of(document.getInteger("year"),
                          document.getInteger("month"));
    }
  }

  @WritingConverter
  static class WriteYearMonthConverter implements Converter<YearMonth, Document> {
    @Override
    public Document convert(YearMonth yearMonth) {
      return new Document().append("year", yearMonth.getYear())
                           .append("month", yearMonth.getMonthValue());
    }
  }

  @WritingConverter
  static class WriteZonedDateTimeConverter implements Converter<ZonedDateTime, Document> {

    @Override
    public Document convert(ZonedDateTime zonedDateTime) {
      return new Document().append("year", zonedDateTime.getYear())
                           .append("month", zonedDateTime.getMonthValue())
                           .append("day", zonedDateTime.getDayOfMonth())
                           .append("hours", zonedDateTime.getHour())
                           .append("minutes", zonedDateTime.getMinute())
                           .append("seconds", zonedDateTime.getSecond())
                           .append("nanos", zonedDateTime.getNano())
                           .append("zone", zonedDateTime.getOffset().getId());
    }
  }

  @ReadingConverter
  static class ReadZonedDateTimeConverter implements Converter<Document, ZonedDateTime> {
    @Override
    public ZonedDateTime convert(Document document) {
      return ZonedDateTime.of(LocalDate.of(document.getInteger("year"),
                                           document.getInteger("month"),
                                           document.getInteger("day")),
                              LocalTime.of(document.getInteger("hours"),
                                           document.getInteger("minutes"),
                                           document.getInteger("seconds"),
                                           document.getInteger("nanos")),
                              ZoneId.of(document.getString("zone")));
    }
  }
}
