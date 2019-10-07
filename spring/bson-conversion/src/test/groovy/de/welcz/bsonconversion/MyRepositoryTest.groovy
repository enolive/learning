package de.welcz.bsonconversion

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.autoconfigure.EnableAutoConfiguration
import org.springframework.boot.test.autoconfigure.data.mongo.DataMongoTest
import org.springframework.test.context.ContextConfiguration
import spock.lang.Specification

import java.time.YearMonth
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.ZonedDateTime

@DataMongoTest
@ContextConfiguration(classes = MongoConverters)
@EnableAutoConfiguration
class MyRepositoryTest extends Specification {
    @Autowired
    private MyRepository repository

    def "document can be saved"() {
        given: 'a document'
        MyDocument document = new MyDocument(ZonedDateTime.now(), YearMonth.now())

        when: 'document is saved'
        MyDocument saved = repository.save(document).block()

        then: 'saved document is returned'
        saved == document
        and: 'id is set'
        saved.id != null
    }

    def "saved document can be read"() {
        given: 'a document'
        ZonedDateTime now = ZonedDateTime.now(ZoneId.from(ZoneOffset.ofHours(2)))
        MyDocument document = new MyDocument(now.withNano(0), YearMonth.now())
        when: 'saved document is read'
        MyDocument read = repository.save(document)
                                    .flatMap { repository.findById(it.id) }
                                    .block()

        then: 'saved document is found'
        read == document
    }
}
