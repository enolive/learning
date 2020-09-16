import 'package:flutter_app/api.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:get_it/get_it.dart';
import 'package:http/http.dart' as http;
import 'package:mockito/mockito.dart';

class MockClient extends Mock implements http.Client {}

void main() {
  ChuckNorrisApi sut;
  http.Client client;
  GetIt getIt = GetIt.instance;

  setUp(() {
    getIt.allowReassignment = true;
    client = MockClient();
    getIt.registerSingleton<http.Client>(client);

    sut = ChuckNorrisApi();
  });

  test('calls chuck norris http service', () async {
    when(client.get(any)).thenAnswer((_) async => http.Response('{}', 200));

    await sut.fetchRandomJoke();

    verify(client.get('https://api.chucknorris.io/jokes/random?category=dev'));
  });

  test('extracts joke from response', () async {
    var json = '{"value": "random joke"}';
    when(client.get(any)).thenAnswer((_) async => http.Response(json, 200));

    final result = await sut.fetchRandomJoke();

    expect(result, 'random joke');
  });
}
