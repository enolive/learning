import { MongoMemoryServer } from 'mongodb-memory-server';
import MongoClient from "mongodb";


async function testMongo() {
  const environment = process.env.NODE_ENV;
  console.log(environment);
  let mongod;
  let uri;
  if (environment !== "production") {
    mongod = new MongoMemoryServer();
    uri = await mongod.getUri();
  } else {
    // use real mongo instead
    uri = "mongodb://127.0.0.1:27017"
  }
  console.log(uri);

  const client = await MongoClient.connect(uri, {useUnifiedTopology: true});
  const db = client.db();
  const todos = db.collection("todos");
  await todos.deleteMany({});
  await todos.insertMany([
    {title: "Chris"},
    {title: "Daniel"},
    {title: "Valentin"},
  ])

  const allTodos = await todos.find().toArray();
  console.log(allTodos);

  await mongod?.stop();
  await client.close();
}

testMongo();


