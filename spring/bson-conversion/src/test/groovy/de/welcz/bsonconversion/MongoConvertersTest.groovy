package de.welcz.bsonconversion

import org.bson.Document
import spock.lang.Specification

import java.time.*

class MongoConvertersTest extends Specification {
    def "year month can be read"() {
        given: 'read year month converter'
        MongoConverters.ReadYearMonthConverter converter = new MongoConverters.ReadYearMonthConverter()
        and: 'an expected year month'
        YearMonth expected = YearMonth.of(2019, Month.MAY)
        and: 'a document with year and month'
        Document document = new Document([year: 2019, month: 5])

        when: 'document is converted to year month'
        YearMonth actual = converter.convert(document)

        then: 'the expected year month is returned'
        actual == expected
    }

    def "year month can be written"() {
        given: 'write year month converter'
        MongoConverters.WriteYearMonthConverter converter = new MongoConverters.WriteYearMonthConverter()
        and: 'an expected document'
        Document expected = new Document([year: 2019, month: 5])
        and: 'a year month'
        YearMonth yearMonth = YearMonth.of(2019, Month.MAY)

        when: 'document is converted to year month'
        Document actual = converter.convert(yearMonth)

        then: 'the expected document is returned'
        actual == expected
    }

    def "zoned date time can be read"() {
        given: 'read zoned date time converter'
        MongoConverters.ReadZonedDateTimeConverter converter = new MongoConverters.ReadZonedDateTimeConverter()
        and: 'a document containing a zoned date time'
        Document document = new Document([year       : 2019,
                                          month      : 5,
                                          day        : 12,
                                          hours      : 7,
                                          minutes    : 44,
                                          seconds    : 4,
                                          nanos      : 122,
                                          zoneSeconds: 2 * 60 * 60])
        and: 'an expected year month'
        ZonedDateTime expected = ZonedDateTime.of(LocalDate.of(2019, Month.MAY, 12),
                                                   LocalTime.of(7, 44, 4, 122),
                                                   ZoneId.from(ZoneOffset.ofHours(2)))

        when: 'document is converted to year month'
        ZonedDateTime actual = converter.convert(document)

        then: 'the expected document is returned'
        actual == expected
    }

    def "zoned date time can be written"() {
        given: 'write zoned date time converter'
        MongoConverters.WriteZonedDateTimeConverter converter = new MongoConverters.WriteZonedDateTimeConverter()
        and: 'an expected document'
        Document expected = new Document([year       : 2019,
                                          month      : 5,
                                          day        : 12,
                                          hours      : 7,
                                          minutes    : 44,
                                          seconds    : 4,
                                          nanos      : 122,
                                          zoneSeconds: 2 * 60 * 60])
        and: 'a year month'
        ZonedDateTime yearMonth = ZonedDateTime.of(LocalDate.of(2019, Month.MAY, 12),
                                                   LocalTime.of(7, 44, 4, 122),
                                                   ZoneId.from(ZoneOffset.ofHours(2)))

        when: 'document is converted to year month'
        Document actual = converter.convert(yearMonth)

        then: 'the expected document is returned'
        actual == expected
    }
}
