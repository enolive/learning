package de.welcz.springmongo.control

import de.welcz.springmongo.entity.MessageDoc
import org.springframework.data.mongodb.repository.MongoRepository

interface MessagesRepository : MongoRepository<MessageDoc, String>
