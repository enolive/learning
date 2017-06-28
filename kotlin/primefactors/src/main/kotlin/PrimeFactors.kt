class PrimeFactors {

    companion object {
        fun of(number: Int): Iterable<Int> {
            val result = Result(number)
            return doItFp(result, 2)
        }

        private fun doItFp(result: Result, factor: Int): Iterable<Int> {
            if (factor > result.remainder) {
                return result.list
            }
            
            result.split(factor)
            return doItFp(result, factor + 1)
        }
    }
}