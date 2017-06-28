class PrimeFactors {

    companion object {
        fun of(number: Int): Iterable<Int> {
            val result = Result(number)
            (2..number).forEach { factor -> result.split(factor) }
            return result.list
        }
    }
}