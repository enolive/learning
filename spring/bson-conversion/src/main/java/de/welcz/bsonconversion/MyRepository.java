package de.welcz.bsonconversion;

import org.bson.types.ObjectId;
import org.springframework.data.mongodb.repository.ReactiveMongoRepository;

@Repository
public interface Repository extends ReactiveMongoRepository<MyDocument, ObjectId> {
}
