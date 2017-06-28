class PrimeFactors {

    companion object {
        fun of(number: Int): Iterable<Int> {
            val result = Result(number)
            for (factor in arrayOf(2, 3, 4, 5)) {
                result.split(factor)
            }
            return result.list
        }
    }
}