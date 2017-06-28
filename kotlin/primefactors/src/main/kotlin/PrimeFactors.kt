class PrimeFactors {

    companion object {
        fun of(number: Int): Iterable<Int> {
            val result = Result(number)
            (2..number).forEach { factor -> result.removeThatShit(factor) }
            return result.list
        }
    }
}