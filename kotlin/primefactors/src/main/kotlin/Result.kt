class Result(val input: Int) {
    private var _remainder = input
    val remainder: Int
        get() = _remainder

    private val _list = ArrayList<Int>()
    val list: Iterable<Int>
        get() = _list

    fun split(factor: Int) {
        if (!canBeFactorized(factor)) {
            return
        }
        addToList(factor)
        removeFromNumber(factor)
        return split(factor)
    }

    fun next(factor: Int): Result {
        val result = Result(input)
        result._list.addAll(list)
        result._remainder = remainder
        result.split(factor)

        return result
    }

    private fun removeFromNumber(factor: Int) {
        _remainder /= factor
    }

    private fun addToList(factor: Int) {
        _list.add(factor)
    }

    private fun canBeFactorized(factor: Int) = remainder % factor == 0

}