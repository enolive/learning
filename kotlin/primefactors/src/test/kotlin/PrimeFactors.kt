class PrimeFactors {

    companion object {
        fun of(number: Int): Iterable<Int> {
            val result = Result(number)
            for (i in arrayOf(2, 3, 5)) {
                result.split(i)
            }
            return result.list
        }
    }
}

class Result(input: Int) {
    private var remainder = input
    private val _list = ArrayList<Int>()

    val list: Iterable<Int>
        get() = _list

    fun split(factor: Int) {
        while (canBeFactorized(factor)) {
            addToList(factor)
            removeFromNumber(factor)
        }
    }

    private fun removeFromNumber(factor: Int) {
        remainder /= factor
    }

    private fun addToList(factor: Int) {
        _list.add(factor)
    }

    private fun canBeFactorized(factor: Int) = remainder % factor == 0

}
