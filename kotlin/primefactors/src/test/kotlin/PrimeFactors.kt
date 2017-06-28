class PrimeFactors {

    companion object {
        fun of(number: Int): ArrayList<Int> {
            val input = number
            val result = Result(input)
            for (i in arrayOf(2, 3, 5)) {
                result.split(i)
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
    val result = this

    fun split(factor: Int) {
        while (canBeFactorized(factor)) {
            add(factor)
            remove(factor)
        }
    }

    private fun remove(factor: Int) {
        result.remainder /= factor
    }

    private fun add(factor: Int) {
        result.list.add(factor)
    }

    private fun canBeFactorized(factor: Int) = result.remainder % factor == 0

}
