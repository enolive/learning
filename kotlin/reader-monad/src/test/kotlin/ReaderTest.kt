import io.kotest.matchers.booleans.shouldBeFalse
import io.kotest.matchers.booleans.shouldBeTrue
import io.kotest.matchers.shouldBe
import net.jqwik.api.*
import net.jqwik.api.constraints.LowerChars
import net.jqwik.api.constraints.NotEmpty
import net.jqwik.api.constraints.UpperChars
import net.jqwik.kotlin.api.any

class ReaderTest {
  val inc = { n: Int -> n + 1 }
  val mult2 = { n: Int -> n * 2 }
  val mult2AndThenInc = mult2.map(inc)

  @Property
  fun `f map g is the pipe composition, that is the same as x to g(f(x))`(@ForAll n: Int) {
    mult2AndThenInc(n) shouldBe inc(mult2(n))
  }

  val isValidCommand = { c: Char -> c in "fblr" }
  val isInvalidCommand = isValidCommand.map(Boolean::not)
  val any = { p: (Char) -> Boolean -> { t: String -> t.any(p) } }
  val and = Boolean::and.curried()
  val containsBothValidAndInvalidCommands1 = any(isValidCommand).map(and).ap(any(isInvalidCommand))
  val containsBothValidAndInvalidCommands2 = any(isValidCommand).zip(any(isInvalidCommand), Boolean::and)

  @Property
  fun `f map(g) ap(h) is the same as x to g(f(x), h(x)) as f ap g is basically x to g(x, f(x))`(@ForAll("validAndInvalidCommands") xs: String) {
    containsBothValidAndInvalidCommands1(xs) shouldBe (xs.any(isValidCommand) && xs.any(isInvalidCommand))
  }

  @Property
  fun `map ap chain behaves like zip`(@ForAll("validAndInvalidCommands") xs: String) {
    // Statistics.collect(containsBothValidAndInvalidCommands1(xs))
    containsBothValidAndInvalidCommands1(xs) shouldBe containsBothValidAndInvalidCommands2(xs)
  }

  @Suppress("unused")
  @Provide
  fun validAndInvalidCommands(): Arbitrary<String> {
    return String.any().withChars("flbrax_ghjkld")
  }

  val isUnique = List<Any>::distinct.flatMap(List<Any>::equals.curried())

  @Property
  fun `f flatMap(g) is the same as x to g(f(x), x)`(@ForAll @NotEmpty xs: List<Any>) {
    // Statistics.collect(isUnique(xs))
    isUnique(xs) shouldBe (xs.distinct() == xs)
  }

  val isUniqueMapAp = List<Any>::distinct.map(List<Any>::equals.curried()).ap(::identity)

  @Property
  fun `flat mapping f and map f ap identity produce the same results`(@ForAll @NotEmpty xs: List<Any>) {
    isUnique(xs) shouldBe isUniqueMapAp(xs)
  }

  val isUniqueZip = List<Any>::distinct.zip(::identity, List<Any>::equals)

  @Property
  fun `flat mapping f and f zip with identity produce the same results`(@ForAll @NotEmpty xs: List<Any>) {
    isUnique(xs) shouldBe isUniqueZip(xs)
  }

  val eqStr = { xs: String -> { ys: String -> xs == ys } }
  val toLower = { xs: String -> xs.lowercase() }
  val isPalindrome = toLower.map(String::reversed.flatMap(eqStr))

  @Property
  fun `is palindrome works`(@ForAll @NotEmpty @UpperChars @LowerChars xs: String) {
    // Statistics.collect(xs.all { it.isLowerCase() }, xs.all { it.isUpperCase()} )
    val comparison = xs.lowercase().let { it.reversed() == it }
    isPalindrome(xs) shouldBe comparison
  }

  @Example
  fun `is palindrome works for examples`() {
    isPalindrome("Anna").shouldBeTrue()
    isPalindrome("Hello").shouldBeFalse()
  }
}
