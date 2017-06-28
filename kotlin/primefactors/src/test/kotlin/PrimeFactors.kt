class PrimeFactors {

    companion object {
        fun of(number: Int): ArrayList<Int> {
            val input = number
            val result = Result(input)
            for (i in arrayOf(2, 3, 5)) {
                split(result, i)
            }
            return result.list
        }

        private fun split(result: Result, factor: Int) {
            while (canBeFactorized(result, factor)) {
                add(result, factor)
                remove(result, factor)
            }
        }

        private fun remove(result: Result, factor: Int) {
            result.remainder /= factor
        }

        private fun add(result: Result, factor: Int) {
            result.list.add(factor)
        }

        private fun canBeFactorized(result: Result, factor: Int) = result.remainder % factor == 0

    }
}

class Result(input: Int) {
    var remainder = input
    val list = ArrayList<Int>()

}
