package de.welcz.springmongo.repository

import de.welcz.springmongo.entity.MessageDoc
import org.springframework.data.mongodb.repository.MongoRepository

interface MessagesRepository : MongoRepository<MessageDoc, String>
