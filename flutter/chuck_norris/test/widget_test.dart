// This is a basic Flutter widget test.
//
// To perform an interaction with a widget in your test, use the WidgetTester
// utility that Flutter provides. For example, you can send tap and scroll
// gestures. You can also use WidgetTester to find child widgets in the widget
// tree, read text, and verify that the values of widget properties are correct.

import 'package:flutter/material.dart';
import 'package:flutter_app/api.dart';
import 'package:flutter_app/main.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:mockito/mockito.dart';

void main() {
  ChuckNorrisApi api;
  MyApp sut;

  setUp(() {
    api = MockApi();
    when(api.fetchRandomJoke()).thenAnswer((_) async => 'random joke');
    sut = MyApp(api: api);
  });

  group('on init', () {
    testWidgets('random joke is fetched', (WidgetTester tester) async {
      await tester.pumpWidget(sut);

      verify(api.fetchRandomJoke());
    });

    testWidgets('random joke is rendered', (WidgetTester tester) async {
      when(api.fetchRandomJoke()).thenAnswer((_) async => 'random joke');
      await tester.pumpWidget(sut);
      await tester.pump();

      expect(find.text('random joke'), findsOneWidget);

      when(api.fetchRandomJoke()).thenAnswer((_) async => 'another joke');
      await tester.tap(find.byIcon(Icons.refresh));
      await tester.pump();

      expect(find.text('another joke'), findsOneWidget);
    });
  });

  group('on refresh', () {
    testWidgets('random joke is fetched', (WidgetTester tester) async {
      await tester.pumpWidget(sut);

      await tester.tap(find.byIcon(Icons.refresh));

      tester.pump();
      verify(api.fetchRandomJoke()).called(2);
    });

    testWidgets('random joke is rendered', (WidgetTester tester) async {
      await tester.pumpWidget(sut);
      when(api.fetchRandomJoke()).thenAnswer((_) async => 'another joke');
      await tester.tap(find.byIcon(Icons.refresh));
      await tester.pump();

      expect(find.text('another joke'), findsOneWidget);
    });
  });
}

class MockApi extends Mock implements ChuckNorrisApi {}
