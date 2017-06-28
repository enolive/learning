class PrimeFactors {

    companion object {
        fun of(number: Int): Iterable<Int> {
            val result = Result(number)
            return doItFp(result, 2)
        }

        private fun doItFp(result: Result, factor: Int): Iterable<Int> {
            if (result.remainder > result.input) {
                return result.list
            }
            
            (factor..result.input).forEach { factor -> result.split(factor) }
            return result.list
        }
    }
}