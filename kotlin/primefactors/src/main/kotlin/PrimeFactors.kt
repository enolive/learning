class PrimeFactors {

    companion object {
        fun of(number: Int): Iterable<Int> {
            val result = Result(number)
            (2..5).forEach { factor -> result.split(factor) }
            return result.list
        }
    }
}