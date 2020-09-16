import 'package:flutter_app/model.dart';
import 'package:flutter_test/flutter_test.dart';

void main() {
  test('json structure can be read', () {
    final map = {
      "value": "random joke",
      "category": "dev",
      "id": 42,
    };
    final expected = JokeResponse(jokeText: 'random joke');

    final result = JokeResponse.fromJson(map);

    expect(result, equals(expected));
  });

  test('invalid structure is ignored', () {
    final Map<String, dynamic> map = {};
    final expected = JokeResponse(jokeText: '');

    final result = JokeResponse.fromJson(map);

    expect(result, equals(expected));
  });
}
