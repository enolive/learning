class PrimeFactors {

    companion object {
        fun of(number: Int): ArrayList<Int> {
            val result = Result(number)
            for (i in arrayOf(2, 3, 5)) {
                result.split(i)
            }
            return result.list
        }
    }
}

class Result(input: Int) {
    var remainder = input
    val list = ArrayList<Int>()

    fun split(factor: Int) {
        while (canBeFactorized(factor)) {
            add(factor)
            remove(factor)
        }
    }

    private fun remove(factor: Int) {
        remainder /= factor
    }

    private fun add(factor: Int) {
        list.add(factor)
    }

    private fun canBeFactorized(factor: Int) = remainder % factor == 0

}
