class PrimeFactors {

    companion object {
        fun of(number: Int): Iterable<Int> {
            return factorize(Result(number), 2)
        }

        private fun factorize(result: Result, factor: Int): Iterable<Int> {
            if (factor > result.remainder) {
                return result.list
            }

            return factorize(result.next(factor), factor + 1)
        }

    }
}