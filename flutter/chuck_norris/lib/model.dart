import 'package:equatable/equatable.dart';

class JokeResponse extends Equatable {
  const JokeResponse({this.jokeText});

  final String jokeText;

  static JokeResponse fromJson(Map<String, dynamic> map) =>
      JokeResponse(jokeText: map['value'] ?? '');

  @override
  List<Object> get props => [jokeText];
}
