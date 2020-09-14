import 'dart:convert';

import 'package:flutter/material.dart';
import 'package:http/http.dart' as http;

void main() {
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
  MyHomePage({Key key, this.title}) : super(key: key);
  final String title;

  @override
  _MyHomePageState createState() => _MyHomePageState();
}

class _MyHomePageState extends State<MyHomePage> {
  var quote = 'Test';

  void _fetchJoke() => fetchRandomJoke().then((jokeText) => setState(() {
        quote = jokeText;
      }));

  Future<String> fetchRandomJoke() => http
      .get('https://api.chucknorris.io/jokes/random?category=dev')
      .then((response) => response.body)
      .then(extractJoke);

  String extractJoke(String body) => jsonDecode(body)['value'];

  @override
  void initState() {
    super.initState();
    _fetchJoke();
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
              Image(image: AssetImage('assets/images/chuck-norris-logo.jpg')),
              Text(
                'Random Wisdom',
                style: Theme.of(context).textTheme.headline4,
              ),
              Text(
                '$quote',
              ),
            ],
          ),
        ),
        floatingActionButton: FloatingActionButton(
          onPressed: _fetchJoke,
          tooltip: 'Refresh Quote',
          child: Icon(Icons.refresh),
        ),
      );
}
