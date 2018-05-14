# Roman numeral conversion

This kata demonstrates how to implement roman numeral conversion
in Java without a single loop by using only fold/unfold from [vavr.io](http://vavr.io).

I used Java 10 to get rid of the static types pollution a little bit
by using type inference, but everything should be also possible with any
JDK >= 8. It isn't possible with pure JDK though as the Streaming API is 
a little bit limited (unless you want to implement fold/unfold by yourself, of course).