import kotlin.reflect.KFunction2

typealias Reader<R, A> = (R) -> A

// monad
fun <R, A> unit(a: A): (R) -> A = { _: R -> a }
fun <R, A, B> Reader<R, A>.flatMap(f: (A) -> Reader<R, B>): Reader<R, B> = { r ->
  f(this(r))(r)
}
// functor -- <&> with flipped version of <$>, to be more in line with kotlin's stdlib
fun <R, A, B> Reader<R, A>.map(f: (A) -> B): Reader<R, B> = this.flatMap { a ->
  unit(f(a))
}
// applicative -- the haskell way, f(a -> b) -> f a -> f b
fun <R, A, B> Reader<R, (A) -> B>.ap(f: Reader<R, A>): Reader<R, B> = this.flatMap { x1 ->
  f.flatMap { x2 -> unit(x1(x2)) }
}
// applicative -- the Kotlin way (eg. .zip)
fun <R, A, B, C> Reader<R, A>.zip(other: Reader<R, B>, f: (A, B) -> C): Reader<R, C> = this.flatMap { x1 ->
  other.flatMap { x2 ->
    unit(f(x1, x2))
  }
}

// some helpers to cope with Kotlin's BS
fun <R, A, B> KFunction2<R, A, B>.curried(): Reader<R, (A) -> B> = { r: R -> { a: A -> this(r, a) } }
fun <R> identity(r: R): R = r

