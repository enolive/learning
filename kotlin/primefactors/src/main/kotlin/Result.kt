class Result(input: Int) {
    private var remainder = input
    private val _list = ArrayList<Int>()

    val list: Iterable<Int>
        get() = _list

    fun split(factor: Int) {
        return splitFp(factor)
        while (canBeFactorized(factor)) {
            addToList(factor)
            removeFromNumber(factor)
        }
    }

    fun splitFp(factor: Int) {
        if (!canBeFactorized(factor)) {
            return
        }
        addToList(factor)
        removeFromNumber(factor)
        return splitFp(factor)
    }

    private fun removeFromNumber(factor: Int) {
        remainder /= factor
    }

    private fun addToList(factor: Int) {
        _list.add(factor)
    }

    private fun canBeFactorized(factor: Int) = remainder % factor == 0

}