import 'package:flutter/material.dart';
import 'package:flutter_app/api.dart';

void main() {
  runApp(MyApp(api: ChuckNorrisApi()));
}

class MyApp extends StatelessWidget {
  const MyApp({Key key, this.api}) : super(key: key);
  final ChuckNorrisApi api;

  @override
  Widget build(BuildContext context) => MaterialApp(
        title: 'Chuck Norris',
        theme: ThemeData(
          primarySwatch: Colors.pink,
          visualDensity: VisualDensity.adaptivePlatformDensity,
        ),
        home: MyHomePage(title: '👊 Chuck Norris App 👊', api: api),
      );
}

class MyHomePage extends StatefulWidget {
  const MyHomePage({Key key, this.title, this.api}) : super(key: key);
  final String title;
  final ChuckNorrisApi api;

  @override
  _MyHomePageState createState() => _MyHomePageState(api: this.api);
}

class _MyHomePageState extends State<MyHomePage> {
  _MyHomePageState({this.api});

  final ChuckNorrisApi api;
  var _jokeText = 'Loading...';

  void _setFetchedJoke() =>
      api
          .fetchRandomJoke()
          .then((jokeText) => setState(() => _jokeText = jokeText));

  @override
  void initState() {
    super.initState();
    _setFetchedJoke();
  }

  @override
  Widget build(BuildContext context) =>
      Scaffold(
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
                style: Theme
                    .of(context)
                    .textTheme
                    .headline4,
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
