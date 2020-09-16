import 'dart:convert';

import 'package:http/http.dart' show Client;

class ChuckNorrisApi {
  Future<String> fetchRandomJoke() => Client()
      .get('https://api.chucknorris.io/jokes/random?category=dev')
      .then((response) => response.body)
      .then(_extractJoke);

  String _extractJoke(String body) => jsonDecode(body)['value'];
}
