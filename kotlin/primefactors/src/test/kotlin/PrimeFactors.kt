class PrimeFactors {
    fun split(i: Int): Iterable<Int> {
        val list = ArrayList<Int>()
        var input = i
        if (input == 3) {
            list.add(3)
        }
        while (input % 2 == 0) {
            list.add(2)
            input /= 2
        }
        return list
    }
}