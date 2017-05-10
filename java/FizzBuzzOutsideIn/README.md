# Fizz Buzz

## Mit Outside-In TDD

* Anderer Ansatz zu Classicist Approach
    * Testen von Verhalten anstatt Zustand
* Gut für Real World Abhängigkeiten
* Weniger gut wenn Algorithmus unklar
* Verwendet Mocks
    * Mock-Frameworks sind von Vorteil! 
    * z.B. Mockito für Java
* Gefahr von Over-Engineering
* Design im Red-Cycle

## Tests

Fizz-Buzz ist implizit abhängig von 3 Regeln. 

* Fizz-Regel (durch 3 teilbar -> Fizz)
* Buzz-Regel (durch 5 teilbar -> Buzz)
* Fizz-Buzz-Regel (durch 3 + 5 teilbar -> Fizz-Buzz)
* Sonst: Zahl selber

Wir versuchen, testgetrieben
den Umgang mit diesen Regeln zu erzwingen. 

## Wie fange ich an?

* Für einen einfachen Fall einen Test klassisch schreiben ("Fake it until you make it")
* Dann explizit nur Mock-Tests schreiben
* Am Ende dann die konkreten Fälle klassich umsetzen (z.B. 3 -> Fizz)