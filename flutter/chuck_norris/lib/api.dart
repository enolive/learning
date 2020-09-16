import 'dart:convert';

import 'package:get_it/get_it.dart';
import 'package:http/http.dart' show Client;

GetIt getIt = GetIt.instance;

class ChuckNorrisApi {
  Client _client = getIt.get<Client>();

  Future<String> fetchRandomJoke() => _client
      .get('https://api.chucknorris.io/jokes/random?category=dev')
      .then((response) => response.body)
      .then(_extractJoke);

  String _extractJoke(String body) => jsonDecode(body)['value'];
}
