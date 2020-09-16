import 'package:flutter_app/api.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:get_it/get_it.dart';
import 'package:http/http.dart';
import 'package:mockito/mockito.dart';

class MockClient extends Mock implements Client {}

void main() {
  ChuckNorrisApi sut;
  Client client;
  GetIt getIt = GetIt.instance;

  setUp(() {
    getIt.allowReassignment = true;
    client = MockClient();
    getIt.registerSingleton<Client>(client);

    sut = ChuckNorrisApi();
  });

  test('calls chuck norris http service', () async {
    var json = '{"value": "random joke"}';
    when(client.get(any)).thenAnswer((_) async => Response(json, 200));

    final result = await sut.fetchRandomJoke();

    verify(client.get('https://api.chucknorris.io/jokes/random?category=dev'));
    expect(result, 'random joke');
  });
}
