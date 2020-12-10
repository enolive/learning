import 'package:flutter/material.dart';
import 'package:chuck_norris/api.dart';
import 'package:get_it/get_it.dart';
import 'package:http/http.dart';

GetIt getIt = GetIt.instance;

void main() {
  getIt
    ..registerSingleton<Client>(Client())
    ..registerSingleton<ChuckNorrisApi>(ChuckNorrisApi());
  runApp(MyApp());
}

class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) => MaterialApp(
        title: 'Chuck Norris',
        theme: ThemeData(
          primarySwatch: Colors.pink,
          visualDensity: VisualDensity.adaptivePlatformDensity,
        ),
        home: MyHomePage(title: '👊 Chuck Norris App 👊'),
      );
}

class MyHomePage extends StatefulWidget {
  const MyHomePage({Key key, this.title, this.api}) : super(key: key);
  final String title;
  final ChuckNorrisApi api;

  @override
  _MyHomePageState createState() => _MyHomePageState();
}

class _MyHomePageState extends State<MyHomePage> {
  var _jokeText = 'Loading...';
  final api = getIt.get<ChuckNorrisApi>();

  void _setFetchedJoke() => api
      .fetchRandomJoke()
      .then((jokeText) => setState(() => _jokeText = jokeText));

  @override
  void initState() {
    super.initState();
    _setFetchedJoke();
  }

  @override
  Widget build(BuildContext context) => Scaffold(
        appBar: AppBar(
          title: Text(widget.title),
        ),
        body: Center(
          child: Column(
            mainAxisAlignment: MainAxisAlignment.start,
            children: <Widget>[
              Image.asset('assets/images/chuck-norris-logo.jpg'),
              Text(
                'Random Wisdom',
                style: Theme.of(context).textTheme.headline4,
              ),
              Text(
                '$_jokeText',
                key: Key('joke'),
              ),
            ],
          ),
        ),
        floatingActionButton: FloatingActionButton(
          onPressed: _setFetchedJoke,
          tooltip: 'Refresh Joke',
          child: Icon(Icons.refresh),
        ),
      );
}
