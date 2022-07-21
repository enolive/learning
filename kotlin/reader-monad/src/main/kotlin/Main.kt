fun main() {
  println("\nFunctor\n")

  val inc = { n: Int -> n + 1 }
  val mult2 = { n: Int -> n * 2 }
  val g1 = mult2.map(inc)

  explain("g1(5)", g1(5))

  println("\nApplicative\n")

  val isValidCommand = { c: Char -> c in "fblr" }
  val isInvalidCommand = isValidCommand.map(Boolean::not)
  val any = { p: (Char) -> Boolean -> { t: String -> t.any(p) } }
  val and = Boolean::and.curried()
  val h1 = any(isValidCommand).map(and).ap(any(isInvalidCommand))
  val h2 = any(isValidCommand).zip(any(isInvalidCommand), Boolean::and)

  explain("""h1("fffaaa")""", h1("fffaaa"))
  explain("""h1("aaa")""", h1("aaa"))
  explain("""h1("fff")""", h1("fff"))
  explain("""h2("fffaaa")""", h2("fffaaa"))
  explain("""h2("aaa")""", h2("aaa"))
  explain("""h2("fff")""", h2("fff"))

  println("\nMonad\n")

  val f1 = List<*>::distinct.flatMap(List<*>::equals.curried())

  explain("""f1(listOf(1, 2, 3))""", f1(listOf(1, 2, 3)))
  explain("""f1(listOf(1, 1, 2, 3, 3))""", f1(listOf(1, 1, 2, 3, 3)))

  println("\nApplicative variants\n")

  val f2 = List<*>::distinct.map(List<*>::equals.curried()).ap(::identity)
  val f3 = List<*>::distinct.zip(::identity, List<*>::equals)

  explain("""f2(listOf(1, 2, 3))""", f2(listOf(1, 2, 3)))
  explain("""f2(listOf(1, 1, 2, 3, 3))""", f2(listOf(1, 1, 2, 3, 3)))
  explain("""f3(listOf(1, 2, 3))""", f3(listOf(1, 2, 3)))
  explain("""f3(listOf(1, 1, 2, 3, 3))""", f3(listOf(1, 1, 2, 3, 3)))

  println("\nAnother fun example\n")

  val eqStr = { xs: String -> { ys: String -> xs == ys } }
  val toLower = { xs: String -> xs.lowercase() }
  val isPalindrome = toLower.map(String::reversed.flatMap(eqStr))
  explain("""isPalindrome("Anna")""", isPalindrome("Anna"))
  explain("""isPalindrome("Hello")""", isPalindrome("Hello"))
}

fun explain(function: String, output: Any) {
  println("$function = $output")
}

fun <T> List<T>.tails() = indices.map { subList(it, size) }
fun <T> List<T>.uniquePairs() =
  tails().flatMap {
    val x = it.first()
    val ys = it.drop(1)
    ys.map { y -> x to y }
  }

fun List<Int>.solve() = uniquePairs().filter { (x, y) -> x + y == 5 }
