class Result(input: Int) {
    private var remainder = input
    private val _list = ArrayList<Int>()

    val list: Iterable<Int>
        get() = _list

    fun removeThatShit(factor: Int) {
        return split(factor)
    }

    fun split(factor: Int) {
        if (!canBeFactorized(factor)) {
            return
        }
        addToList(factor)
        removeFromNumber(factor)
        return split(factor)
    }

    private fun removeFromNumber(factor: Int) {
        remainder /= factor
    }

    private fun addToList(factor: Int) {
        _list.add(factor)
    }

    private fun canBeFactorized(factor: Int) = remainder % factor == 0

}