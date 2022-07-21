import kotlin.reflect.KFunction2

typealias Reader<R, A> = (R) -> A

// monad
fun <R, A> unit(a: A): Reader<R, A> = { _: R -> a }
fun <R, A, B> Reader<R, A>.flatMap(f: (A) -> Reader<R, B>): Reader<R, B> = { r ->
  f(this(r))(r)
}
// as a monad is automatically also a functor and applicative, these functions are derived from flatMap

// functor -- flipped version compared to haskell (<&> instead of <$>), to be more in line with kotlin's stdlib
// m.map(f) === m.flatMap { x -> unit(f(x)) }
fun <R, A, B> Reader<R, A>.map(f: (A) -> B): Reader<R, B> = this.flatMap { a ->
  unit(f(a))
}
// applicative -- the haskell way, f(a -> b) -> f a -> f b
// see also Hoogle https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#t:Monad
// m1.ap(m2) === m1.flatMap { x1 -> m2.flatMap { x2 -> unit(x1(x2)) }
// since unit and pure are the same and Kotlin does not support higher kind type classes, I skipped the definition of pure
fun <R, A, B> Reader<R, (A) -> B>.ap(f: Reader<R, A>): Reader<R, B> = this.flatMap { x1 ->
  f.flatMap { x2 -> unit(x1(x2)) }
}
// applicative -- the Kotlin way (like the different .zip functions).
// See how similar the implementation is to the standard applicative!
// note: this is basically Haskell's liftA2 :: (a -> b -> c) -> f a -> f b -> f c
// the only difference is that the lifted function comes last (and is uncurried)
fun <R, A, B, C> Reader<R, A>.zip(other: Reader<R, B>, f: (A, B) -> C): Reader<R, C> = this.flatMap { x1 ->
  other.flatMap { x2 ->
    unit(f(x1, x2))
  }
}

// some helpers to cope with Kotlin's BS. I deliberately did not add arrow-kt,
// which has already those definitions to limit the dependencies
fun <R, A, B> KFunction2<R, A, B>.curried(): Reader<R, (A) -> B> = { r: R -> { a: A -> this(r, a) } }
fun <R> identity(r: R): R = r

