package de.welcz.bsonconversion;

import org.bson.types.ObjectId;
import org.springframework.data.mongodb.repository.ReactiveMongoRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface MyRepository extends ReactiveMongoRepository<MyDocument, ObjectId> {
}
