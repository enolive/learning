class PrimeFactors {

    companion object {
        fun of(number: Int): Iterable<Int> {
            val result = Result(number)
            return doItFp(result)
        }

        private fun doItFp(result: Result): Iterable<Int> {
            (2..result.input).forEach { factor -> result.split(factor) }
            return result.list
        }
    }
}