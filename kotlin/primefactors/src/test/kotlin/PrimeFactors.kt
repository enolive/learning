class PrimeFactors {
    fun split(i: Int): Iterable<Int> {
        val list = ArrayList<Int>()
        var input = i
        while (input % 2 == 0) {
            list.add(2)
            input /= 2
        }
        return list
    }
}