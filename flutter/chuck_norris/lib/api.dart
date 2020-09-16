import 'dart:convert';

import 'package:flutter_app/model.dart';
import 'package:get_it/get_it.dart';
import 'package:http/http.dart' as http;

GetIt getIt = GetIt.instance;

class ChuckNorrisApi {
  final _client = getIt.get<http.Client>();

  Future<String> fetchRandomJoke() => _client
      .get('https://api.chucknorris.io/jokes/random?category=dev')
      .then((response) => response.body)
      .then(jsonDecode)
      .then((map) => JokeResponse.fromJson(map))
      .then((joke) => joke.jokeText);
}
