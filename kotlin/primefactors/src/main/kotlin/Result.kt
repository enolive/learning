class Result(val input: Int) {
    private constructor(result: Result, factor: Int) : this(result.input) {
        _list.addAll(result.list)
        _remainder = result.remainder
        split(factor)
    }
    
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
        val nextResult = Result(this, factor)
        return nextResult
    }

    private fun removeFromNumber(factor: Int) {
        _remainder /= factor
    }

    private fun addToList(factor: Int) {
        _list.add(factor)
    }

    private fun canBeFactorized(factor: Int) = remainder % factor == 0

}